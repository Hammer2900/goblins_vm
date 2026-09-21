;;; Copyright 2026 Jessica Tallon
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
(define-module (goblins ocapn netlayer noise)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib inbox)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer base-port)
  #:use-module (goblins ocapn netlayer utils)
  #:use-module (goblins utils base32)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils crypto)
  #:use-module (goblins contrib syrup)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (noise)
  #:export (^noise-netlayer
            noise-netlayer-env))

(define noise-protocol-name "Noise_XK_25519_AESGCM_SHA256")

(define (ocapn-peer->key ocapn-peer)
  (base32-decode (ocapn-peer-designator ocapn-peer)))

(define (static-key->swiss-num static-key)
  (sha256 static-key))

(define* (spawn-noise-connection our-static-keypair initiator? our-outbox
                                 #:optional
                                 remote-side-key)
  ;; The inbox for **encrypted** messages.
  (define-values (our-inbox their-outbox stop-inbox!)
    (spawn-inbox))

  (define-values (write-msg! read-msg! %encrypt %decrypt)
    (make-noise-connection noise-protocol-name initiator?
                           #:s our-static-keypair
                           #:rs remote-side-key))

  (define-values (message-io-vow message-io-resolver)
    (spawn-promise-and-resolver))

  (define (^message-io bcom encrypt decrypt)
    (methods
     ((read-message unmarshallers)
      (on (<- our-inbox)
          (lambda (encrypted-msg)
            (define msg (decrypt encrypted-msg))
            (syrup-decode msg #:unmarshallers unmarshallers))
          #:promise? #t))
     ((write-message msg marshallers)
      (define encoded-msg (syrup-encode msg #:marshallers marshallers))
      (<-np our-outbox (encrypt encoded-msg)))))

  ;; Handshake helpers
  (define (spawn-message-io c1 c2)
    (define-values (encrypt-cs decrypt-cs)
      (if initiator?
          (values c1 c2)
          (values c2 c1)))
    (define (encrypt msg)
      (%encrypt encrypt-cs msg))
    (define (decrypt msg)
      (%decrypt decrypt-cs msg))
    (spawn ^message-io encrypt decrypt))

  (define (read-next-handshake-message)
    (on (<- our-inbox)
        (lambda (msg)
          (define-values (_msg c1 c2)
            (read-msg! msg))
          (if (and c1 c2)
              (<-np message-io-resolver 'fulfill (spawn-message-io c1 c2))
              (write-handshake-message!)))))

  (define (write-handshake-message!)
    (define-values (msg c1 c2) (write-msg!))
    (<-np our-outbox msg)
    (if (and c1 c2)
        (<-np message-io-resolver 'fulfill (spawn-message-io c1 c2))
        (read-next-handshake-message)))

  ;; If we're the initiator, we should send a message, otherwise listen for
  ;; theirs.
  (if initiator?
      (write-handshake-message!)
      (read-next-handshake-message))
  (values message-io-vow their-outbox))

;; This is basically a cell which spawns with a promise in and doesn't keep its
;; value between aurie restores. It acts kind of like a promise and resolve in
;; one for our purposes. We want it so that the ^noise-netlayer can communicate
;; the connection establisher to the ^noise-connector.
(define-actor (^promise-cell bcom)
  #:portrait (lambda () '())
  (define-values (vow resolver)
    (spawn-promise-and-resolver))
  (methods
   ((current-value) vow)
   ((fulfill new-value)
    ($ resolver 'fulfill new-value))))

(define-actor (^noise-connector bcom our-static-key conn-establisher)
  (lambda (our-outbox)
    (define-values (message-io-vow their-outbox)
      (spawn-noise-connection our-static-key #f our-outbox))
    (<- (<- conn-establisher 'current-value) message-io-vow #f)
    their-outbox))

(define* (^noise-netlayer bcom netlayer
                          #:key
                          (name 'noise)
                          (static-keypair (generate-x25519-keypair)))
  (define mycapn (spawn-mycapn netlayer))
  (define conn-establisher-promise-cell (spawn ^promise-cell))
  (define noise-connector
    (spawn ^noise-connector static-keypair conn-establisher-promise-cell))
  (spawn ^noise-netlayer* name mycapn netlayer static-keypair
         conn-establisher-promise-cell
         noise-connector))

(define-actor (^noise-netlayer* bcom name mycapn netlayer static-keypair
                                shared-conn-establisher
                                noise-connector)
  (define-values (our-private-key our-public-key)
    (match static-keypair
      ((private . public) (values private public))))

  (define swiss-num (static-key->swiss-num our-public-key))
  (define nonce-registry (<- mycapn 'get-registry))
  (<-np nonce-registry 'register noise-connector swiss-num)
  (define our-location-vow
    (on (<- netlayer 'our-location)
        (lambda (transport-location)
          (define noise-connector-sref
            (make-ocapn-sturdyref transport-location swiss-num))
          (make-ocapn-peer
           name
           (base32-encode (car static-keypair))
           (hashmap ("connector" (ocapn-id->string noise-connector-sref)))))
        #:promise? #t))

  (methods
   ((netlayer-name) 'noise)
   ((our-location) our-location-vow)
   ((self-location? location)
    (on our-location-vow
        (lambda (our-location)
          (same-peer-location? our-location location))
        #:promise? #t))
   ((setup conn-establisher)
    (<-np shared-conn-establisher 'fulfill conn-establisher))
   ((connect-to remote-peer)
    ;; Spawn the noise connection objects. We don't have our outbox until we
    ;; tell the otherside, so for now use a promise.
    (define-values (our-outbox-vow our-outbox-resolver)
      (spawn-promise-and-resolver))
    (define remote-side-key (ocapn-peer->key remote-peer))
    (define-values (message-io their-outbox)
      (spawn-noise-connection static-keypair #t our-outbox-vow
                              remote-side-key))

    ;; Reach out and inform the other side there's a new connection
    (define remote-noise-connector-sref
      (string->ocapn-id
       (hashmap-ref (ocapn-peer-hints remote-peer) "connector")))
    (define remote-connector-vow
      (<- mycapn 'enliven remote-noise-connector-sref))
    (on (<- remote-connector-vow their-outbox)
        (lambda (our-outbox)
          (<-np our-outbox-resolver 'fulfill our-outbox)
          (define conn-establisher-vow (<- shared-conn-establisher 'current-value))
          (<- conn-establisher-vow message-io remote-peer))
        #:catch
        (lambda (err)
          (<-np our-outbox-resolver 'break err))
        #:promise? #t))))

(define noise-netlayer-env
  (persistence-env-compose
   (make-persistence-env
    `((((goblins ocapn netlayer noise) ^noise-netlayer) ,^noise-netlayer*)
      (((goblins ocapn netlayer noise) ^noise-connector) ,^noise-connector)
      (((goblins ocapn netlayer noise) ^promise-cell) ,^promise-cell)))
   captp-env))