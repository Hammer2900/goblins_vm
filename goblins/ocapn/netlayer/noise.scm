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
  #:use-module (rnrs bytevectors gnu)
  #:use-module (ice-9 match)
  #:use-module (noise)
  #:export (^noise-netlayer
            noise-netlayer-env))

(define noise-protocol-name "Noise_XK_25519_AESGCM_SHA256")

(define (ocapn-peer->key ocapn-peer)
  (base32-decode (ocapn-peer-designator ocapn-peer)))

;; Strigine hack, it's pretty slow. (see hoot issue #920)
(cond-expand
 (guile)
 (hoot
  (define (bytevector-slice bv offset size)
    ;; Obviously this is not a "view" into bv, changing bv won't change this.
    ;; We're using it because it's fast
    (define new-bv (make-bytevector size))
    (bytevector-copy! bv offset new-bv 0 size)
    new-bv)))

(define (noise-chunk-and-encrypt noise plaintext-bv)
  ;; https://noiseprotocol.org/noise.html#message-format
  ;; Messages must be 65535 or less, it always have 16b of authentication data
  (define noise-max-chunk-size (- 65535 16))
  (define bv-length (bytevector-length plaintext-bv))
  (if (< bv-length noise-max-chunk-size)
      (noise-encode noise plaintext-bv)
      (cons
       bv-length
       (let lp ((i 0))
         (if (< i bv-length)
             (let ((k (min (- bv-length i) noise-max-chunk-size)))
               (cons (noise-encode noise (bytevector-slice plaintext-bv i k))
                     (lp (+ i k))))
             '())))))

(define (noise-decrypt-and-dechunk noise message)
  (match message
    ((? bytevector?) (noise-decode noise message))
    ((length-prefix . chunks)
     (let ((bv (make-bytevector length-prefix)))
       (let lp ((index 0)
                (remaining chunks))
         (match remaining
           (() bv)
           ((next-chunk . rest)
            (let* ((decrypted (noise-decode noise next-chunk))
                   (decrypted-length (bytevector-length decrypted)))
              (bytevector-copy! decrypted 0 bv index decrypted-length)
              (lp (+ index decrypted-length) rest)))))))))

(define* (spawn-noise-connection our-static-keypair initiator? our-outbox
                                 #:optional
                                 remote-side-key)
  ;; The inbox for **encrypted** messages.
  (define-values (our-inbox their-outbox stop-inbox!)
    (spawn-inbox))

  (define noise (make-noise noise-protocol-name initiator?
                            #:static-key our-static-keypair
                            #:remote-static-key remote-side-key))

  (define-values (message-io-vow message-io-resolver)
    (spawn-promise-and-resolver))

  (define (^message-io bcom)
    (methods
     ((read-message unmarshallers)
      (let-on ((encrypted-msg (<- our-inbox)))
        (syrup-decode (noise-decrypt-and-dechunk noise encrypted-msg)
                      #:unmarshallers unmarshallers)))
     ((write-message msg marshallers)
      (define encoded-msg (syrup-encode msg #:marshallers marshallers))
      (<-np our-outbox (noise-chunk-and-encrypt noise encoded-msg)))))

  (define (read-next-handshake-message)
    (let-on ((msg (<- our-inbox)))
      (noise-decode noise msg)
      (if (noise-handshake-complete? noise)
          (<-np message-io-resolver 'fulfill (spawn ^message-io))
          (write-handshake-message!))))

  (define (write-handshake-message!)
    (<-np our-outbox (noise-encode noise #vu8()))
    (if (noise-handshake-complete? noise)
        (<-np message-io-resolver 'fulfill (spawn ^message-io))
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
  (spawn ^noise-netlayer* name mycapn netlayer static-keypair
         conn-establisher-promise-cell noise-connector))

(define-actor (^noise-netlayer* bcom name mycapn netlayer static-keypair
                                shared-conn-establisher
                                noise-connector)
  (define our-public-key (car static-keypair))
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
   ((netlayer-name) name)
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
