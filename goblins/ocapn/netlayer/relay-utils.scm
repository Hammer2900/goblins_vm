;;; Copyright 2023-2026 Jessica Tallon
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

(define-module (goblins ocapn netlayer relay-utils)
  #:use-module (goblins)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer relay)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins actor-lib timers)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils crypto)
  #:use-module (goblins contrib base64)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:export (^relay-admin
            fetch-and-spawn-relay-netlayer
            relay-utils-env))

(define-actor (^relay-admin bcom warden mycapn
                            client-netlayer-name
                            federation-netlayer-name
                            #:key
                            (accounts (spawn ^ghash))
                            (ephemeral-accounts (spawn ^ghash))
                            (allow-registrations? #f)
                            (allow-ephemeral-accounts? #f)
                            (ephemeral-account-lifespan (* 60 60 24)))
  "Allows for creating new relay netlayer accounts with a name

It has several methods, some of which are considered admin behaviour and are
warded behind @var{warden}. The rest of the behaviour are available to anyone
with a reference to the ^relay-admin object. The methods are:

Admin:

@itemize
@item
@code{register-account label} Register a new account with the given label. It
will return a sturdyref to a cell containing the endpoint and controller
sturdyrefs registered against the @var{client-netlayer-name} netlayer.
@item
@code{remove-account label} Remove the account associated with the @var{label}.
@item
@code{get-accounts} Returns a hashmap containing the accounts with some account
data.
@item
@code{configure #:allow-registrations? #:allow-ephemeral-accounts? #:ephemeral-account-lifespan}
Reconfigure the admin object to the new settings.
@end itemize

Remaining behaviour:

@itemize
@item
@code{allow-registrations?} Returns if registrations are allowed to be
registered.
@item
@code{allow-ephemeral-accounts?} Returns if ephemeral accoutns are allowed to be
registered.
@item
@code{register} Registers a new account on the relay returning the sturdyref to
a read-only cell which contains the endpoint and controller sturdyrefs of the
new account.
@item
@code{register-ephemeral} Registers a new ephemeral account returning the
sturdyref of a read-only cell containing the endpoint and controller sturdyrefs
of the new account. This account will be removed after the
@var{ephemeral-account-lifespan} time has elapsed. This should only be used for
short lived accounts."
  (define registry (<- mycapn 'get-registry))
  (define* (register-account label #:key (type 'admin-registered))
    (when ($ accounts 'ref label)
      (error "Account with this label already exists" label))
    ;; Since we don't set the account until the let-on, register 'pending
    ;; to prevent it being re-registered.
    ($ accounts 'set label (list 'pending #f #f #f))
    
    ;; Spawn the relay pair
    (define-values (endpoint controller)
      (spawn-relay-pair (facet mycapn 'enliven)))
    
    ;; This is a little cell to make it easier to get both objects. We use swiss
    ;; nums as they are netlayer independent.
    (let-on ((endpoint-sref
              (<- mycapn 'register endpoint federation-netlayer-name))
             (controller-sref
              (<- mycapn 'register controller client-netlayer-name)))
      (match ($ accounts 'ref label)
        ;; remove-account will mark it for deletion for us to clean up if it's
        ;; been deleted between pending and getting the sturdyrefs.
        ('deleted
         (<-np registry 'remove endpoint)
         (<-np registry 'remove controller)
         ($ accounts 'remove label)
         (when (eq? type 'ephemeral)
           ($ ephemeral-accounts 'remove label)))
        ;; If it's still pending we're good, finish setting up.
        (('pending #f #f #f)
         (let* ((account-container
                 (spawn ^cell (list endpoint-sref controller-sref)))
                (ro-account-container (cell->read-only account-container))
                (account-container-sref
                 (<- mycapn 'register ro-account-container client-netlayer-name)))
           ($ accounts 'set label
              (list type account-container endpoint controller))
           account-container-sref))
        (_ (error "Account could not be registered")))))
  (define (remove-account label)
    (match ($ accounts 'ref label)
      (('pending #f #f #f)
       ;; Mark it so register-account knows.
       ($ accounts 'set 'deleted))
      ((account-type account-container endpoint controller)
       ;; TODO: Disable controller.
       (<-np registry 'remove endpoint)
       (<-np registry 'remove controller)
       ($ account-container 'disabled)
       ($ accounts 'remove label)
       (when (eq? account-type 'ephemeral)
         ($ ephemeral-accounts 'remove label))
       #t)
      (#f
       (error "Account with label does not exist" label))))
  (define admin-beh
    (methods
     (register-account register-account)
     (remove-account remove-account)
     ((configure #:key
                 (allow-registrations? allow-registrations?)
                 (allow-ephemeral-accounts? allow-ephemeral-accounts?)
                 (ephemeral-account-lifespan ephemeral-account-lifespan))
      ;; If we've disabled ephemeral accounts, remove them all now since the
      ;; ephemeral account sweep only occurs when ephemeral accounts are enabled
      (unless allow-ephemeral-accounts?
        (hashmap-for-each
         (lambda (label _registration-time)
           (remove-account label)
           ($ ephemeral-accounts 'remove label))
         ($ ephemeral-accounts 'data)))
      (bcom (^relay-admin bcom warden mycapn
                          client-netlayer-name
                          federation-netlayer-name
                          #:accounts accounts
                          #:ephemeral-accounts ephemeral-accounts
                          #:allow-registrations? allow-registrations?
                          #:allow-ephemeral-accounts? allow-ephemeral-accounts?
                          #:ephemeral-account-lifespan ephemeral-account-lifespan)))
     ((get-accounts) ($ accounts 'data))))
     
  (define public-beh
    (methods
     ((allow-registrations?) allow-registrations?)
     ((allow-ephemeral-accounts?) allow-ephemeral-accounts?)
     ((register)
      (unless allow-registrations?
        (error "Public account registration is disabled"))
      ;; Don't let the user's pick their own, it may reveal info we don't want to
      ;; share. We'll register it until some random label.
      (define label
        (string-append "public-" (base64-encode (strong-random-bytes 32))))
      (register-account label #:type 'user-registered))
     ;; This is an account which is not designed to be kept around.
     ((register-ephemeral)
      (unless allow-ephemeral-accounts?
        (error "Ephemeral account registration is disabled"))
      ;; Ideally this would be smarter, for example it could keep it around
      ;; until the account is idle 24h or more then clear it up. For now let's
      ;; keep the account alive for 24h and then remove it.
      (define label
        (string-append "ephemeral-" (base64-encode (strong-random-bytes 32))))
      (let-on ((account (register-account label #:type 'ephemeral)))
        ($ ephemeral-accounts 'set label (current-time))
        account))))

  ;; Ephemeral account cleanup
  (define (ephemeral-account-sweep . args)
    (on (<- ephemeral-accounts 'data)
        (lambda (ephemeral-account-data)
          (define now (current-time))
          (hashmap-for-each
           (lambda (label registration-time)
             (when (< now (+ registration-time ephemeral-account-lifespan))
               (remove-account label)))
           ephemeral-account-data))
        #:finally
        (lambda ()
          ;; re-trigger after an amount of time relative to the lifespan of an
          ;; ephemeral account.
          (on (timeout (/ ephemeral-account-lifespan 12))
              ephemeral-account-sweep))))
       
  (when allow-ephemeral-accounts?
    (ephemeral-account-sweep))
  
  (ward warden admin-beh #:extends public-beh))
     
     
(define* (fetch-and-spawn-relay-netlayer netlayer mycapn account-setup-sref)
  "Retrieves account from account-setup-sref and provides relay-netlayer"
  ;; The account sturdyref is pointing at a read only cell containing the
  ;; sturdyrefs for both the endpoint and controller endpoints
  (define account-setup-container-vow (<- mycapn 'enliven account-setup-sref))
  (on-match (<- account-setup-container-vow)
    ((endpoint-sref controller-sref)
     (spawn ^relay-netlayer mycapn endpoint-sref controller-sref))
    ('disabled
     (error "Relay account has been disabled"))))

(define relay-utils-env
  (persistence-env-compose
   (namespace-env '(goblins ocapn netlayer relay-utils)
     ^relay-admin)
   relay-env cell-env ward-env))

