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
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins actor-lib ward)
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
  #:use-module (ice-9 binary-ports)
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
    ;; Obviously this isn't a faithful representation of what `bytevector-slice`
    ;; does on the Guile VM. That doesn't matter. We're mainly picking it
    ;; doesn't require allocating and copying. We don't have this on Hoot so
    ;; this will do for now, see Hoot's issue #920 for more info.
    (define new-bv (make-bytevector size))
    (bytevector-copy! bv offset new-bv 0 size)
    new-bv)))

(define (noise-chunk-and-encrypt noise plaintext-bv)
  ;; https://noiseprotocol.org/noise.html#message-format
  ;; Messages must be 65535 or less, they always have 16b of authentication data
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

(define (noise-decrypt-and-dechunk noise length chunks)
  (define bv (make-bytevector length))
  (let lp ((index 0)
           (remaining chunks))
    (match remaining
      (() bv)
      ((next-chunk . rest)
       (let* ((decrypted (noise-decode noise next-chunk))
              (decrypted-length (bytevector-length decrypted)))
         (bytevector-copy! decrypted 0 bv index decrypted-length)
         (lp (+ index decrypted-length) rest))))))

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

  ;; Watch for severance between us and the remote peer.
  (define-values (severed-vow severed-resolver)
    (spawn-promise-and-resolver))

  (let-on ((our-outbox our-outbox))
    (when (remote-refr? our-outbox)
      (on-sever our-outbox
                (lambda (type reason)
                  (<-np severed-resolver 'fulfill (eof-object))
                  (<-np stop-inbox!)))))

  (define (^message-io bcom)
    (methods
     ((read-message unmarshallers)
      (define (decode bv)
        (syrup-decode bv #:unmarshallers unmarshallers))
      (on-match (race (<- our-inbox) severed-vow)
        ((? bytevector? bv) (decode (noise-decode noise bv)))
        ((length . chunks)
         (decode (noise-decrypt-and-dechunk noise length chunks)))
        ((? eof-object? obj) obj)))
     ((write-message msg marshallers)
      (define encoded-msg (syrup-encode msg #:marshallers marshallers))
      (<-np our-outbox (noise-chunk-and-encrypt noise encoded-msg)))))

  (define (read-next-handshake-message)
    (let-on ((msg (<- our-inbox)))
      ;; While we *could* use payloads in handshake messages, they have weaker
      ;; security properties than post-handshake. We send null bytevectors until
      ;; the handshake is complete.
      (noise-decode noise msg)
      (if (noise-handshake-complete? noise)
          (<-np message-io-resolver 'fulfill (spawn ^message-io))
          (write-next-handshake-message))))

  (define (write-next-handshake-message)
    (<-np our-outbox (noise-encode noise #vu8()))
    (if (noise-handshake-complete? noise)
        (<-np message-io-resolver 'fulfill (spawn ^message-io))
        (read-next-handshake-message)))

  ;; If we're the initiator, we should send a message, otherwise listen for
  ;; theirs.
  (if initiator?
      (write-next-handshake-message)
      (read-next-handshake-message))
  (values message-io-vow their-outbox))

(define-actor (^noise-connector bcom our-static-key warden)
  (define-values (conn-establisher-vow conn-establisher-resolver)
    (spawn-promise-and-resolver))
  ;; The main behavior, used by the remote side to start a new connection
  (define (remote-beh our-outbox)
    (define-values (message-io-vow their-outbox)
      (spawn-noise-connection our-static-key #f our-outbox))
    (<- conn-establisher-vow  message-io-vow #f)
    their-outbox)
  ;; Used by the ^noise-netlayer to give us access to MyCapN's conn-establisher
  (define (admin-beh conn-establisher)
    (<-np conn-establisher-resolver 'fulfill conn-establisher))
  (ward warden admin-beh #:extends remote-beh))

(define* (^noise-netlayer bcom netlayer
                          #:key
                          (name 'noise)
                          (static-keypair (generate-x25519-keypair)))
  (define mycapn (spawn-mycapn netlayer))
  (define-values (warden incanter)
    (spawn-warding-pair))
  (define noise-connector
    (spawn ^noise-connector static-keypair warden))
  (spawn ^noise-netlayer* name mycapn netlayer static-keypair incanter
         noise-connector))

(define-actor (^noise-netlayer* bcom name mycapn netlayer static-keypair
                                incanter noise-connector)
  (define-values (conn-establisher-vow conn-establisher-resolver)
    (spawn-promise-and-resolver))
  (define our-public-key (car static-keypair))
  (define swiss-num (sha256 our-public-key))
  (define registry (<- mycapn 'get-registry))
  (<-np registry 'register noise-connector swiss-num)
  (define our-location-vow
    (let-on ((transport-location (<- netlayer 'our-location)))
      (make-ocapn-peer
       name
       ;; Designator is our base32 encoded static public key.
       (base32-encode our-public-key)
       ;; Hints are a sturdyref of our connector so the other side can enliven
       ;; it and use it to initiate new connections to us.
       (let ((connector-sref (make-ocapn-sturdyref transport-location swiss-num)))
         (hashmap ("connector" (ocapn-id->string connector-sref)))))))

  (methods
   ((netlayer-name) name)
   ((our-location) our-location-vow)
   ((self-location? location)
    (let-on ((our-location our-location-vow))
      (same-peer-location? our-location location)))
   ((setup conn-establisher)
    (<-np conn-establisher-resolver 'fulfill conn-establisher)
    (<-np incanter noise-connector conn-establisher))
   ((connect-to remote-peer)
    ;; To create a new connection we'll use spawn-noise-connection which spawns
    ;; some inbox actors to allow the responder to deliver messages and sets up
    ;; everything to allow for the Noise handshake to occur and then encryption
    ;; and decryption of messages.
    ;; We'll then reach out to the responder's ^noise-connector (a sort of
    ;; bootstrap actor) and hand them an outbox actor where it can send messages
    ;; meant for us. They'll setup it's own inbox and Noise machinery.
    ;; Since spawn-noise-connection takes an outbox to deliver messages to,
    ;; we'll spawn a promise, and fulfill it with the outbox when we've sent
    ;; message to the remote ^noise-connector.
    (define-values (our-outbox-vow our-outbox-resolver)
      (spawn-promise-and-resolver))
    (define remote-side-key (ocapn-peer->key remote-peer))
    (define-values (message-io remote-side-outbox)
      (spawn-noise-connection static-keypair #t our-outbox-vow
                              remote-side-key))

    ;; Reach out and inform the responder, to inform them of the new connection
    (define remote-noise-connector-sref
      (string->ocapn-id
       (hashmap-ref (ocapn-peer-hints remote-peer) "connector")))
    (define remote-connector-vow
      (<- mycapn 'enliven remote-noise-connector-sref))
    (let-on ((our-outbox (<- remote-connector-vow remote-side-outbox)))
      (<-np our-outbox-resolver 'fulfill our-outbox)
      (<- conn-establisher-vow message-io remote-peer)))))

(define noise-netlayer-env
  (persistence-env-compose
   (make-persistence-env
    `((((goblins ocapn netlayer noise) ^noise-netlayer) ,^noise-netlayer*)
      (((goblins ocapn netlayer noise) ^noise-connector) ,^noise-connector)))
   captp-env
   ward-env))
