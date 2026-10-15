;;; Copyright 2023 Christine Lemmer-Webber
;;; Copyright 2024 Jessica Tallon
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define-module (tests ocapn netlayer test-relay)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (goblins ocapn netlayer relay)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib on)
  #:use-module (tests utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64))

(test-begin "test-relay")

(define fakenl-vat (spawn-vat #:name "fakenl-vat"))
(define fakenl-network
  (with-vat fakenl-vat
    (spawn ^fake-network)))

(define (spawn-vat-in-fakenl name)
  (define new-vat (spawn-vat #:name (string-append name "-vat")))
  (define location (string->ocapn-id (format #f "ocapn://~a.fake" name)))
  (define netlayer
    (with-vat new-vat
      (spawn-fake-netlayer fakenl-network name)))
  (values new-vat location netlayer))

(define-values (a-vat a-loc a-netlayer)
  (spawn-vat-in-fakenl "alice-m"))
(define-values (b-vat b-loc b-netlayer)
  (spawn-vat-in-fakenl "bob-m"))
(define-values (relay-vat relay-loc relay-netlayer)
  (spawn-vat-in-fakenl "relay"))
(define relay-mycapn
  (with-vat relay-vat
    (spawn-mycapn relay-netlayer)))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define bob-greeter
  (with-vat b-vat
    (spawn ^greeter "Bob")))

;;; Setting up endpoints and controllers for Alice/Bob on the relay

(define-values (ra-endpoint ra-controller)
  (with-vat relay-vat
    (spawn-relay-pair (spawn ^facet relay-mycapn 'enliven))))

(define-values (ra-endpoint-sref ra-controller-sref)
  (with-vat relay-vat
    (values ($ relay-mycapn 'register ra-endpoint 'fake)
            ($ relay-mycapn 'register ra-controller 'fake))))

(define-values (rb-endpoint rb-controller)
  (with-vat relay-vat
    (spawn-relay-pair (spawn ^facet relay-mycapn 'enliven))))

(define-values (rb-endpoint-sref rb-controller-sref)
  (with-vat relay-vat
    (values ($ relay-mycapn 'register rb-endpoint 'fake)
            ($ relay-mycapn 'register rb-controller 'fake))))

;;; Now to create the relay and register it with Alice and Bob's MyCapN object.
(define a-relay-netlayer
  (with-vat a-vat
    (let-on ((ra-endpoint-sref ra-endpoint-sref)
             (ra-controller-sref ra-controller-sref))
      (spawn ^relay-netlayer
             (spawn-mycapn a-netlayer)
             ra-endpoint-sref
             ra-controller-sref))))

(define a-mycapn
  (with-vat a-vat
    (let-on ((a-relay-netlayer a-relay-netlayer))
      (spawn-mycapn a-relay-netlayer))))

(define b-relay-netlayer
  (with-vat b-vat
    (let-on ((rb-endpoint-sref rb-endpoint-sref)
             (rb-controller-sref rb-controller-sref))
      (spawn ^relay-netlayer
             (spawn-mycapn b-netlayer)
             rb-endpoint-sref
             rb-controller-sref))))

(define b-mycapn
  (with-vat b-vat
    (let-on ((b-relay-netlayer b-relay-netlayer))
      (spawn-mycapn b-relay-netlayer))))

(define bob-greeter-sref
  (with-vat b-vat
    (<- b-mycapn 'register bob-greeter 'relay)))

(test-assert "Relay netlayer sturdyref resolves to a remote reference"
  (match (resolve-vow-and-return-result
          a-vat
          (lambda ()
            (<- a-mycapn 'enliven bob-greeter-sref)))
    (#(ok (? remote-object-refr?)) #t)
    (_ #f)))

(test-assert "Simple messaging over the relay netlayer works"
  (match (resolve-vow-and-return-result
          a-vat
          (lambda ()
            (<- (<- a-mycapn 'enliven bob-greeter-sref) "Alice")))
    (#(ok "Hello Alice, my name is Bob!") #t)
    (_ #f)))

(define (setup-relay name)
  "Sets up both own relay server and relay client"
  (define-values (vat loc netlayer)
    (spawn-vat-in-fakenl (string-append "relay-" name)))
  (define mycapn
    (with-vat vat
      (let-on ((netlayer netlayer))
        (spawn-mycapn netlayer))))
  (define-values (endpoint controller)
    (with-vat vat
      (spawn-relay-pair (spawn ^facet mycapn 'enliven))))
  (define-values (endpoint-sref controller-sref)
    (with-vat vat
      (values (<- mycapn 'register endpoint 'fake)
              (<- mycapn 'register controller 'fake))))
  (define-values (client-vat client-loc client-netlayer)
    (spawn-vat-in-fakenl (string-append "client-" name)))
  (define relay-netlayer
    (with-vat client-vat
      (let-on ((endpoint-sref endpoint-sref)
               (controller-sref controller-sref))
        (spawn ^relay-netlayer
               (spawn-mycapn client-netlayer)
               endpoint-sref
               controller-sref))))
  (define client-mycapn
    (with-vat client-vat
      (let-on ((relay-netlayer relay-netlayer))
        (spawn-mycapn relay-netlayer))))
  (values client-vat client-mycapn client-netlayer netlayer))


;; Register the relay with each side
(define-values (c-vat c-mycapn _cc-netlayer _cs-netlayer)
  (setup-relay "carol"))
(define-values (d-vat d-mycapn _dd-netlayer _ds-netlayer)
  (setup-relay "debra"))

(define c-greeter
  (with-vat c-vat
    (spawn ^greeter "Carol")))

(define c-greeter-sref-vow
  (with-vat c-vat
    (<- c-mycapn 'register c-greeter 'relay)))

(define d-greeter-vow
  (with-vat d-vat
    (<- d-mycapn 'enliven c-greeter-sref-vow)))

(test-assert "Federated Relay netlayer sturdyref resolves to a remote reference"
  (match (resolve-vow-and-return-result
          d-vat
          (lambda ()
            (<- d-mycapn 'enliven c-greeter-sref-vow)))
    (#(ok (? remote-object-refr?)) #t)
    (_ #f)))

(test-assert "Simple messaging over the federated relay netlayer works"
  (match (resolve-vow-and-return-result
          d-vat
          (lambda ()
            (<- (<- d-mycapn 'enliven c-greeter-sref-vow) "Debra")))
    (#(ok "Hello Debra, my name is Carol!") #t)
    (_ #f)))

;; Test on-sever
;; The first case we're going to test is when alice on A is connected to her
;; relay and bob on b is connected on his relay, if alice severs her connection
;; to her relay, her refrs and bob's refrs should trigger on-sever using the
;; relay.

(define-values (e-vat e-mycapn ec-netlayer es-netlayer)
  (setup-relay "elsa"))

(define-values (f-vat f-mycapn fc-netlayer fs-netlayer)
  (setup-relay "frank"))

(define elsa-greeter
  (with-vat e-vat
    (spawn ^greeter "Elsa")))
(define elsa-greeter-sref
  (with-vat e-vat
    (<- e-mycapn 'register elsa-greeter 'relay)))

(define frank-greeter
  (with-vat f-vat
    (spawn ^greeter "Frank")))
(define frank-greeter-sref
  (with-vat f-vat
    (<- f-mycapn 'register frank-greeter 'relay)))

;; Due to https://codeberg.org/spritely/goblins/issues/659
;; we should enliven ensure we don't end up in a crossed hellos situation, in
;; order to avoid it, we'll enliven one, then enliven the other.
(define-values (e-done-enlivening-vow e-done-enlivening-resolver)
  (with-vat e-vat
    (spawn-promise-and-resolver)))
(define frank-on-e-vow
  (with-vat e-vat
    (on (<- e-mycapn 'enliven frank-greeter-sref)
        (lambda (frank-greeter)
          ($ e-done-enlivening-resolver 'fulfill #t)
          frank-greeter)
        #:promise? #t)))
(define elsa-on-f-vow
  (with-vat f-vat
    (on e-done-enlivening-vow
        (lambda _
          (<- f-mycapn 'enliven elsa-greeter-sref))
        #:promise? #t)))

;; Break e's netlayer to her relay
(define test-vat (spawn-vat))
(define-values (frank-on-e-severed-vow frank-on-e-resolver)
  (with-vat test-vat
    (spawn-promise-and-resolver)))
(define-values (elsa-on-f-severed-vow elsa-on-f-resolver)
  (with-vat test-vat
    (spawn-promise-and-resolver)))

(with-vat test-vat
  (on (all-of frank-on-e-vow elsa-on-f-vow)
      (match-lambda
        ((frank-on-e elsa-on-f)
         (on-sever elsa-on-f
                   (lambda (type reason)
                     ($ elsa-on-f-resolver 'fulfill (list type reason))))
         (on-sever frank-on-e
                   (lambda (type reason)
                     ($ frank-on-e-resolver 'fulfill (list type reason))))
         (<-np ec-netlayer 'halt)))))

(test-equal "When relay client netlayer halts, severance works for clients refrs"
  #(ok (disconnect "Remote disconnected"))
  (resolve-vow-and-return-result
   test-vat
   (lambda ()
     frank-on-e-severed-vow)))

(test-equal "When other side relay client netlayer disconnects, severance works for our clients refrs"
  #(ok (disconnect "Remote disconnected"))
  (resolve-vow-and-return-result
   test-vat
   (lambda ()
     elsa-on-f-severed-vow)))

;; Test when the two relays disconnect
(define-values (g-vat g-mycapn gc-netlayer gs-netlayer)
  (setup-relay "gary"))

(define-values (h-vat h-mycapn hc-netlayer hs-netlayer)
  (setup-relay "hannah"))

(define gary-greeter
  (with-vat g-vat
    (spawn ^greeter "Gary")))
(define gary-greeter-sref
  (with-vat g-vat
    (<- g-mycapn 'register gary-greeter 'relay)))

(define hannah-greeter
  (with-vat h-vat
    (spawn ^greeter "Hannah")))
(define hannah-greeter-sref
  (with-vat h-vat
    (<- h-mycapn 'register hannah-greeter 'relay)))

;; Like above, avoid crossed hellos for now.
(define-values (h-done-enlivening-vow h-done-enlivening-resolver)
  (with-vat h-vat
    (spawn-promise-and-resolver)))
(define hannah-on-g-vow
  (with-vat g-vat
    (on (<- g-mycapn 'enliven hannah-greeter-sref)
        (lambda (hannah-greeter)
          (<-np h-done-enlivening-resolver 'fulfill #t)
          hannah-greeter)
        #:promise? #t)))
(define gary-on-h-vow
  (with-vat h-vat
    (on h-done-enlivening-vow
        (lambda _
          (<- h-mycapn 'enliven gary-greeter-sref))
        #:promise? #t)))

(define-values (hannah-on-g-severed-vow hannah-on-g-resolver)
  (with-vat test-vat
    (spawn-promise-and-resolver)))
(define-values (gary-on-h-severed-vow gary-on-h-resolver)
  (with-vat test-vat
    (spawn-promise-and-resolver)))

(with-vat test-vat
  (on (all-of hannah-on-g-vow gary-on-h-vow)
      (match-lambda
        ((hannah-on-g gary-on-h)
         (on-sever hannah-on-g
                   (lambda (type reason)
                     ($ hannah-on-g-resolver 'fulfill (list type reason))))
         (on-sever gary-on-h
                   (lambda (type reason)
                     ($ gary-on-h-resolver 'fulfill (list type reason))))
         ;; Halt the netlayer gary's relay is using, this should break the connections
         ;; between the relays
         (<-np gs-netlayer 'halt)))))

(test-equal "When sever happens between two federating relay servers, on-sever should reach clients"
  #(ok ((disconnect "Remote disconnected") (disconnect "Remote disconnected")))
  (resolve-vow-and-return-result
   test-vat
   (lambda ()
     (all-of elsa-on-f-severed-vow frank-on-e-severed-vow))))

;; We want to verify that a netlayer client (^relay-netlayer) will automatically
;; attempt to reconnect if a severance happens, since we can't restart the fake
;; netlayer, we'll have the severance happen at the server side and reconstruct
;; the relay server.
(define-values (reconnect-server-vat reconnect-server-loc
                                     reconnect-server-netlayer)
  (spawn-vat-in-fakenl "reconnect-relay"))
(define reconnect-server-mycapn
  (with-vat reconnect-server-vat
    (spawn-mycapn reconnect-server-netlayer)))
(define-values (reconnect-endpoint reconnect-controller)
  (with-vat reconnect-server-vat
    (spawn-relay-pair (spawn ^facet reconnect-server-mycapn 'enliven))))
(define-values (reconnect-endpoint-sref reconnect-controller-sref)
  (with-vat reconnect-server-vat
    (values ($ reconnect-server-mycapn 'register reconnect-endpoint 'fake)
            ($ reconnect-server-mycapn 'register reconnect-controller 'fake))))
(define-values (reconnect-client-vat reconnect-client-loc
                                     reconnect-client-netlayer)
  (spawn-vat-in-fakenl "reconnect-netlayer"))
(define reconnect-relay-netlayer
  (with-vat reconnect-client-vat
    (let-on ((reconnect-endpoint-sref reconnect-endpoint-sref)
             (reconnect-controller-sref reconnect-controller-sref))
      (spawn ^relay-netlayer
             (spawn-mycapn reconnect-client-netlayer)
             reconnect-endpoint-sref
             reconnect-controller-sref))))
(define reconnect-client-mycapn
  (with-vat reconnect-client-vat
    (let-on ((reconnect-relay-netlayer reconnect-relay-netlayer))
      (spawn-mycapn reconnect-relay-netlayer))))

;; Make an object that we will be able to connect to.
(define reconnect-greeter
  (with-vat reconnect-client-vat
    (spawn ^greeter "Reconnect")))

(define reconnect-greeter-sref
  (with-vat reconnect-client-vat
    (<- reconnect-client-mycapn 'register reconnect-greeter 'relay)))

;; To properly test we've reconnected later on, we of course need to connect
;; first to ensure there actually is a connection between
;; client <-> relay server
;; Once there is we should then halt the server's relay netlayer and make a
;, new one as explained below. Make a throw away client to get the sturdyref
;; then check we've halted the relay server netlayer.
(define halted-server-netlayer?
  (let-values (((vat mycapn client-netlayer server-netlayer)
                (setup-relay "throwaway")))
    (with-vat vat
      (on (<- mycapn 'enliven reconnect-greeter-sref)
          (lambda (greeter)
            ;; We now know the client and server are connected, as we have the refr.
            ;; now just wait until we've halted
            (<- reconnect-server-netlayer 'halt))
          #:promise? #t))))

;; We need to do this manually, not using spawn-vat-in-fakenl because if we setup
;; and install ourselves before the above happens, we clobber them causing
;; problems.
(define reconnect-server-vat*
  (spawn-vat #:name "reconnect-relay*"))
(define-values (network-vow network-resolver)
  (call-with-vat reconnect-server-vat* spawn-promise-and-resolver))
(define-values (reconnect-server-netlayer* reconnect-server-mycapn*)
  (let ((loc "ocapn://reconnect-relay.fake")
        (netlayer (with-vat reconnect-server-vat*
                    (spawn-fake-netlayer network-vow "reconnect-relay"))))
    (with-vat reconnect-server-vat*
      (values netlayer
              (spawn-mycapn netlayer)))))

(define reconnect-ready?
  (with-vat reconnect-server-vat*
    (on halted-server-netlayer?
        (lambda _
          ;; We're ready so we can finish hooking up the rest of the netlayer.
          ($ network-resolver 'fulfill fakenl-network)
          ;; As we cannot restart the netlayer, we need to make a new one and install
          ;; the endpoint and controller at the same place we had them before.
          (define-values (reconnect-endpoint* reconnect-controller*)
            (spawn-relay-pair (spawn ^facet reconnect-server-mycapn* 'enliven)))

          (define reconnect-server-registry
            ($ reconnect-server-mycapn* 'get-registry))
          (on (all-of reconnect-endpoint-sref reconnect-controller-sref)
              (match-lambda
                ((endpoint-sref controller-sref)
                 (define endpoint-swiss-num
                   (ocapn-sturdyref-swiss-num endpoint-sref))
                 (define controller-swiss-num
                   (ocapn-sturdyref-swiss-num controller-sref))
                 ($ reconnect-server-registry 'register reconnect-endpoint* endpoint-swiss-num)
                 ($ reconnect-server-registry 'register reconnect-controller* controller-swiss-num)
                 #t))
              #:promise? #t))
        #:promise? #t)))

;; Now whats left to do is try and connect
(let-values (((vat mycapn client-netlayer server-netlayer)
              (setup-relay "testing")))
  (test-equal "Test relay reconnects and remains reachable after connection breakage"
    #(ok "Hello testing, my name is Reconnect!")
    (resolve-vow-and-return-result
     test-vat
     (lambda ()
       ;; Have to make sure we've waited until it's halted before we reconnect.
       (define reconnected-greeter-vow
         (on reconnect-ready?
             (lambda _
               (<- mycapn 'enliven reconnect-greeter-sref))
             #:promise? #t))
       (<- reconnected-greeter-vow "testing")))))

(test-end "test-relay")
