;;; Copyright 2023-2024 Jessica Tallon
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

(define-library (goblins ocapn netlayer relay-utils)
  (export ^relay-admin
          fetch-and-spawn-relay-netlayer
          relay-utils-env)

  (import (guile)
          (goblins)
          (goblins utils hashmap)
          (goblins utils ghash)
          (goblins ocapn captp)
          (goblins ocapn ids)
          (goblins ocapn netlayer relay)
          (goblins ocapn netlayer websocket)
          (goblins actor-lib methods)
          (goblins actor-lib joiners)
          (goblins actor-lib facet)
          (goblins actor-lib on)
          (fibers channels)
          (ice-9 match))

  (cond-expand
   (guile
    (import (goblins ocapn netlayer onion)
            (goblins ocapn netlayer tcp-tls)))
   (hoot))

  (begin
    (define-actor (^relay-account bcom enliven register #:key [setup? #f])
      (define (setup-beh)
        (error "Already setup"))
      (define (main-beh)
        (define-values (relay-endpoint relay-controller)
          (spawn-relay-pair enliven))
        (bcom (^relay-account bcom enliven register #:setup? #t)
              (all-of
               (<- register 'register relay-endpoint)
               (<- register 'register relay-controller))))
      (if setup?
          setup-beh
          main-beh))

    (define-actor (^relay-admin bcom enliven register
                                #:optional [accounts (make-ghash)])
      "Allows for creating new relay netlayer accounts with a name

It has two methods, the first `add-account' takes a name and creates a relay
account for that name. It provides a sturdyref back which can be used by the
client exactly once to configure and setup the relay.

The second method is `get-accounts' which lists all the account names that have
been created on this relay-admin."

      (methods
       [(add-account name)
        (when (hashmap-ref accounts name)
          (error "Account with name already exists" name))
        (define new-account (spawn ^relay-account enliven register))
        (bcom (^relay-admin bcom enliven register
                            (hashmap-set accounts name new-account))
              (<- register 'register new-account))]
       [(get-accounts)
        (hashmap-fold
         (lambda (name revoke account-list)
           (cons name account-list))
         (list)
         accounts)]))

    (define* (fetch-and-spawn-relay-netlayer account-setup-sref
                                             #:key
                                             [netlayer #f]
                                             [mycapn #f])
      "Retrieves account from account-setup-sref and provides relay-netlayer"
      (define account-setup-peer
        (ocapn-sturdyref-peer account-setup-sref))
      (define base-netlayer
        (or netlayer
            (cond-expand
             (guile
              (match (ocapn-peer-transport account-setup-peer)
                ('onion (spawn ^onion-netlayer))
                ('tcp-tls (spawn ^tcp-tls-netlayer "localhost"))
                ('websocket (spawn ^websocket-netlayer))))
             (hoot
              (match (ocapn-peer-transport account-setup-peer)
                ((or 'onion 'tcp-tls) (error "Not supported under hoot"))
                ('websocket (spawn ^websocket-netlayer)))))))

      ;; While most OCapN connections normally would expect connections to many
      ;; different peers and support for handoffs between those, this situation
      ;; is a bit different.  We're just looking for a connection between this
      ;; peer and the relay "server", this is what this mycapn object is that
      ;; we're setting up. We should not expect any shortening or connection to
      ;; ourselves problems with this setup.
      (define base-mycapn
        (or mycapn (spawn-mycapn base-netlayer)))

      ;; Enliven the "setup" sturdyref, then setup the relay netlayer with that.
      (define account-setup-vow
        (<- base-mycapn 'enliven account-setup-sref))
      (define account-vow (<- account-setup-vow))
      (define relay-endpoint-sref-vow
        (on-match account-vow
          ((relay-endpoint-sref _relay-controller-sref)
           relay-endpoint-sref)))
      (define relay-controller-sref-vow
        (on-match account-vow
          ((_relay-endpoint-sref relay-controller-sref)
           relay-controller-sref)))
      (spawn ^relay-netlayer
             base-mycapn
             relay-endpoint-sref-vow
             relay-controller-sref-vow))

    (define relay-utils-env
      (persistence-env-compose
       (namespace-env '(goblins ocapn netlayer relay-utils)
         ^relay-account ^relay-admin)
       relay-env))))
