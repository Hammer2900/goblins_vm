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
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer relay)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins actor-lib timers)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins actor-lib opportunistic)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins utils crypto)
  #:use-module (goblins utils base32)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:export (^relay-admin
            fetch-and-spawn-relay-netlayer
            relay-utils-env))

;; This is here, rather than ^relay-admin because the restore procedure needs to
;; be able to use it. It's really part of the ^relay-admin though.
(define (remove-account label accounts registry)
  (define $/<- (select-$/<- accounts))
  (on-match ($/<- accounts 'ref label)
    (('pending #f #f #f)
     ;; Mark it so register-account knows.
     ($/<- accounts 'set 'deleted))
    ((account-type account-container endpoint controller _)
     ;; TODO: Disable controller.
     (<-np registry 'remove endpoint)
     (<-np registry 'remove controller)
     ($ account-container 'disabled)
     ($/<- accounts 'remove label)
     #t)
    (#f
     (error "Account with label does not exist" label))))

(define-actor (^relay-admin bcom warden mycapn
                            client-netlayer-name
                            federation-netlayer-name
                            #:key
                            (accounts (spawn ^ghash)))
;;   "Allows for creating new relay netlayer accounts with a name

;; It has several methods, some of which are considered admin behaviour and are
;; warded behind @var{warden}. The rest of the behaviour are available to anyone
;; with a reference to the ^relay-admin object. The methods are:

;; Admin:

;; @itemize
;; @item
;; @code{register-account label} Register a new account with the given label. It
;; will return a sturdyref to a cell containing the endpoint and controller
;; sturdyrefs registered against the @var{client-netlayer-name} netlayer.
;; @item
;; @code{remove-account label} Remove the account associated with the @var{label}.
;; @item
;; @code{get-accounts} Returns a hashmap containing the accounts with some account
;; data.
;; @end itemize

;; Remaining behaviour:

;; @itemize
;; @item
;; @code{allow-registrations?} Returns if registrations are allowed to be
;; registered.
;; @item
;; @code{register expiration} Registers a new account on the relay returning the
;; sturdyref to a read-only cell which contains the endpoint and controller
;; sturdyrefs of the new account. The @var{expiration} in seconds determines when
;; the account is automatically-removed. If @var{expiration} is set to @code{#f},
;; the account will not expire."
  #:restore
  (lambda* (_version warden mycapn client-netlayer-name federation-netlayer-name
                     #:key (accounts (spawn ^ghash)))
    (define admin
      (spawn ^relay-admin warden mycapn client-netlayer-name
             federation-netlayer-name
             #:accounts accounts))
    (define registry (<- mycapn 'get-registry))
    ;; We need to remove the ephemeral accounts.
    (on (<- accounts 'data)
        (lambda (data)
          (hashmap-for-each
           (match-lambda*
             ((label ('pending _ _ _ _))
              (<-np admin 'remove-account label))
             ((label 'deleted) (<-np accounts 'remove label))
             ((label (_ _ _ _ (? number?)))
              ;; An ephemeral account, these should be perged after a restore.
              (remove-account label accounts registry))
             (_ #f))
           data)))
    admin)
  (define registry (<- mycapn 'get-registry))
  (define* (register-account label
                             #:key
                             (type 'admin-registered)
                             (expiration #f))
    (when ($ accounts 'ref label)
      (error "Account with this label already exists" label))
    ;; Since we don't set the account until the let-on, register 'pending
    ;; to prevent it being re-registered.
    ($ accounts 'set label (list 'pending #f #f #f #f))

    ;; Spawn the relay pair
    (define-values (endpoint controller)
      (spawn-relay-pair (facet mycapn 'enliven)))

    ;; Determine the time we'd like to expire the account.
    (define expiration-time (and expiration (+ (current-time) expiration)))
    ;; Setup a timer to delete it
    (when expiration
      (on (timeout expiration)
          (lambda _
            (remove-account label accounts registry))))

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
         ($ accounts 'remove label))
        ;; If it's still pending we're good, finish setting up.
        (('pending _ _ _ _)
         (let* ((account-container
                 (spawn ^cell (list endpoint-sref controller-sref)))
                (ro-account-container (cell->read-only account-container))
                (account-container-sref
                 (<- mycapn 'register ro-account-container client-netlayer-name)))
           ($ accounts 'set label
              (list type account-container endpoint controller expiration-time))
           account-container-sref))
        (_ (error "Account could not be registered")))))
  (define admin-beh
    (methods
     (register-account register-account)
     ((remove-account label) (remove-account label accounts registry))
     ((get-accounts) ($ accounts 'data))))

  (define public-beh
    (methods
     ((register #:optional expiration)
      ;; Don't let the user's pick their own, it may reveal info we don't want
      ;; to share. We'll register it until some random label.
      (define label
        (string-append "public-"
                       (base32-encode (strong-random-bytes 32) #:pad? #f)))
      (register-account label
                        #:type 'user-registered
                        #:expiration expiration))))

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
   relay-env captp-env cell-env ward-env))
