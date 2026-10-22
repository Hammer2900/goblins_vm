;;; Copyright 2021-2022 Christine Lemmer-Webber
;;; Copyright 2022-2026 Jessica Tallon
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

(define-module (goblins ocapn netlayer fake)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module ((goblins core) #:hide ($))
  #:use-module ((goblins core) #:select ($) #:prefix $)
  #:use-module (goblins define-actor)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib inbox)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins contrib syrup)
  #:use-module (ice-9 match)
  #:export (^fake-network ^fake-netlayer fake-netlayer-env spawn-fake-netlayer))

(define-actor (^fake-network _bcom #:optional (routes (spawn ^ghash)))
  (methods
   ((register name dest-connection-outbox)
    ($$ routes 'set name dest-connection-outbox))
   ((connect-to name)
    (define dest-connection-outbox
      ($$ routes 'ref name))
    (when (not (live-refr? dest-connection-outbox))
      (error (format #t "No connection found by name: ~a" name)))
    (define-values (me-enq-ch me-deq-ch me-stop?)
      (spawn-delivery-agent))
    (define-values (them-enq-ch them-deq-ch them-stop?)
      (spawn-delivery-agent))
    (<-np dest-connection-outbox `(*incoming-new-conn* ,me-enq-ch ,them-deq-ch))
    `(*outgoing-new-conn* ,me-deq-ch ,them-enq-ch))))

;; Does not need to be aurie persistable as it's only used for live connections
;; which don't get persisted.
(define (^message-io bcom incoming-ch outgoing-ch)
  (define incoming-io (spawn ^io incoming-ch))
  (define outgoing-io (spawn ^io outgoing-ch))
  (define halt? (make-condition))

  (define (read-message ch)
    (perform-operation
     (choice-operation
      (get-operation ch)
      (wrap-operation (wait-operation halt?)
                      (lambda () the-eof-object)))))

  (define halted-beh
    (methods
     [(read-message unmarshallers) the-eof-object]
     [(write-message msg marshallers) *unspecified*]
     [(halt) *unspecified*]))
  (define main-beh
    (methods
     [(halt)
      ;; Propagate the halt to the otherside...
      (<-np outgoing-io
            (lambda (ch)
              (put-message ch the-eof-object)
              *unspecified*))
      ;; Signal the condition to halt and tell the io actors to stop.
      (signal-condition! halt?)
      (<-np incoming-io 'halt)
      (<-np outgoing-io 'halt)
      ;; Become some defunct behavior.
      (bcom halted-beh)]
     [(read-message unmarshallers)
      (<- incoming-io
          (lambda (ch)
            (match (read-message ch)
              ((? eof-object? eof) eof)
              (msg (syrup-decode msg #:unmarshallers unmarshallers)))))]
     [(write-message msg marshallers)
      (<-np outgoing-io
            (lambda (ch)
              (put-message ch (syrup-encode msg #:marshallers marshallers))
              *unspecified*))]))
  main-beh)


(define-actor (^fake-netlayer bcom our-name network new-conn-inbox new-conn-outbox)
  (define our-location (make-ocapn-peer 'fake our-name #f))
  (define-values (halted-vow halted-resolver)
    (spawn-promise-and-resolver))
  (define (start-listening conn-establisher)
    (on (<- new-conn-inbox)
        (match-lambda
          (('*incoming-new-conn* them-enq-ch me-deq-ch)
           (define message-io
             (spawn ^message-io me-deq-ch them-enq-ch))
           (on halted-vow
               (lambda _
                 (<-np message-io 'halt)))
           (<-np-extern conn-establisher message-io #f)))
        #:finally
        (lambda ()
          (start-listening conn-establisher))))

  (define* (netlayer-beh #:optional conn-establisher)
    (methods
     [(netlayer-name) 'fake]
     [(self-location? loc)
      (same-peer-location? our-location loc)]
     [(our-location) our-location]
     [(setup conn-establisher)
      (start-listening conn-establisher)
      (bcom (netlayer-beh conn-establisher))]
     [(halt) ($$ halted-resolver 'fulfill #t)]
     [(connect-to remote-peer)
      (match remote-peer
        (($ <ocapn-peer> 'fake name _)
         (on (<- network 'connect-to name)
             (match-lambda
               (('*outgoing-new-conn* me-deq-ch them-enq-ch)
                (define message-io
                  (spawn ^message-io me-deq-ch them-enq-ch))
                (on halted-vow
                    (lambda _
                      (<-np message-io 'halt)))
                (<- conn-establisher message-io remote-peer)))
             #:promise? #t)))]))
  (netlayer-beh))

;; Provide a little helper to spawn a netlayer
(define (spawn-fake-netlayer network name)
  (define-values (new-conn-inbox new-conn-outbox _new-conn-halt)
    (spawn-inbox))
  (<-np network 'register name new-conn-outbox)
  (spawn ^fake-netlayer name network new-conn-inbox new-conn-outbox))

(define fake-netlayer-env
  (persistence-env-compose
   (make-persistence-env
    `((((goblins ocapn netlayer fake) ^fake-netlayer) ,^fake-netlayer)
      (((goblins ocapn netlayer fake) ^fake-network) ,^fake-network)))
   inbox-env common-env))

