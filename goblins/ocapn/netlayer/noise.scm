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
  #:use-module (goblins actor-lib on)
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
      (let-on ((encrypted-msg (<- our-inbox)))
        (syrup-decode (decrypt encrypted-msg) #:unmarshallers unmarshallers)))
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
    (let-on ((msg (<- our-inbox)))
      (define-values (_msg c1 c2)
        (read-msg! msg))
      (if (and c1 c2)
          (<-np message-io-resolver 'fulfill (spawn-message-io c1 c2))
          (write-handshake-message!))))

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
  (spawn ^noise-netlayer* name mycapn netlayer public-key
         conn-establisher-promise-cell noise-connector))

(define-actor (^noise-netlayer* bcom name mycapn netlayer static-keypair
                                shared-conn-establisher
                                noise-connector)
  (define our-public-key (cdr static-keypair))
  (define swiss-num (sha256 our-public-key))
  (define nonce-registry (<- mycapn 'get-registry))
  (<-np nonce-registry 'register noise-connector swiss-num)
  (define our-location-vow
    (let-on ((transport-location (<- netlayer 'our-location)))
      (make-ocapn-peer
       name
       ;; Designator is the our static public key base32 encoded.
       (base32-encode our-public-key)
       ;; Hints are a swiss-num of the transport netlayer pointing to the noise
       ;; connector actor. This allows other peers to connect to us.
       (let ((connector-sref (make-ocapn-sturdyref transport-location swiss-num)))
         (hashmap ("connector" (ocapn-id->string connector-sref)))))))

  (methods
   ((netlayer-name) 'noise)
   ((our-location) our-location-vow)
   ((self-location? location)
    (let-on ((our-location our-location-vow))
      (same-peer-location? our-location location)))
   ((setup conn-establisher)
    (<-np shared-conn-establisher 'fulfill conn-establisher))
   ((connect-to remote-peer)
    ;; To create a new connection we'll use spawn-noise-connection which spawns
    ;; some inbox actors to allow the otherside to deliver messages and sets up
    ;; everything to allow for the Noise handshake to occur and then encryption
    ;; and decryption of messages.
    ;; We'll then reach out to the other side's ^noise-connector (a sort of
    ;; bootstrap actor) and hand them an outbox actor where it can send messages
    ;; meant for us. It'll setup it's own inbox and Noise machinary.
    ;; Since spawn-noise-connection takes an outbox to deliver messages to,
    ;; we'll spawn a promise, and fulfill it with the outbox when we've sent
    ;; message to the remote ^noise-connector.
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
    (<-np our-outbox-resolver 'fulfill (<- remote-connector-vow their-outbox))
    (let-on ((our-outbox (<- remote-connector-vow their-outbox)))
      (define conn-establisher-vow
        (<- shared-conn-establisher 'current-value))
      (<- conn-establisher-vow message-io remote-peer)))))

(define noise-netlayer-env
  (persistence-env-compose
   (make-persistence-env
    `((((goblins ocapn netlayer noise) ^noise-netlayer) ,^noise-netlayer*)
      (((goblins ocapn netlayer noise) ^noise-connector) ,^noise-connector)
      (((goblins ocapn netlayer noise) ^promise-cell) ,^promise-cell)))
   captp-env))