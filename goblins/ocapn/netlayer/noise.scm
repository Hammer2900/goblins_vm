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
(define-library (goblins ocapn netlayer noise)
  (export ^noise-netlayer noise-netlayer-env)
  (import (guile)
          (goblins core)
          (goblins define-actor)
          (goblins actor-lib inbox)
          (goblins actor-lib methods)
          (goblins ocapn ids)
          (goblins ocapn captp)
          (goblins ocapn netlayer base-port)
          (goblins ocapn netlayer utils)
          (goblins utils base32)
          (goblins utils hashmap)
          (goblins contrib syrup)
          (rnrs bytevectors))
  (cond-expand
   (hoot (import (noise hoot)))
   (guile (import (noise gnutls))))
  (begin
    (define noise-protocol-name "Noise_XK_25519_AESGCM_SHA256")

    (define (ocapn-peer->key ocapn-peer)
      (base32-decode (ocapn-peer-designator ocapn-peer)))

    (define* (spawn-noise-connection our-static-keypair initiator?
                                     our-outbox
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

    ;; This is the object we export to the otherside to allow them to establish
    ;; a connection to us.
    (define (^noise-connector bcom our-static-key conn-establisher-vow)
      (lambda (our-outbox)
        (define-values (message-io-vow their-outbox)
          (spawn-noise-connection our-static-key #f our-outbox))
        (<-np conn-establisher-vow message-io-vow #f)
        their-outbox))

    (define* (^noise-netlayer bcom netlayer
                              #:key
                              (name 'noise)
                              (static-keypair (generate-x25519-keypair)))
      (spawn ^noise-netlayer* name (spawn-mycapn netlayer) netlayer
             static-keypair))
    (define-actor (^noise-netlayer* bcom name mycapn netlayer static-keypair)
      (define-values (our-location-vow our-location-resolver)
        (spawn-promise-and-resolver))

      (define-values (conn-establisher-vow conn-establisher-resolver)
        (spawn-promise-and-resolver))

      (on (<- mycapn 'register
              (spawn ^noise-connector static-keypair conn-establisher-vow)
              ($ netlayer 'netlayer-name))
          (lambda (noise-connector-sref)
            (<-np our-location-resolver 'fulfill
                  (make-ocapn-peer
                   'noise
                   (base32-encode (car static-keypair))
                   ;; TODO Needs to be a hash
                   (hashmap ("connector" (ocapn-id->string noise-connector-sref)))))))

      (methods
       ((netlayer-name) 'noise)
       ((our-location) our-location-vow)
       ((self-location? location)
        (on our-location-vow
            (lambda (our-location)
              (same-peer-location? our-location location))
            #:promise? #t))
       ((setup conn-establisher)
        ($ conn-establisher-resolver 'fulfill conn-establisher))
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
              (<- conn-establisher-vow message-io remote-peer))
            #:catch
            (lambda (err)
              (<-np our-outbox-resolver 'break err))
            #:promise? #t))))

    (define noise-netlayer-env
      (persistence-env-compose
       (make-persistence-env
        `((((goblins ocapn netlayer noise) ^noise-netlayer) ,^noise-netlayer*)))
       captp-env))))