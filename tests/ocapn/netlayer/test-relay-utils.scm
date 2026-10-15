;;; Copyright 2023 Jessica Tallon
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


(define-module (tests ocapn netlayer test-relay-utils)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (goblins ocapn netlayer relay-utils)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins actor-lib timers)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins persistence-store memory)
  #:use-module (tests utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-64))

(test-begin "test-relay-utils")

(define fake-network-vat (spawn-vat #:name "interwebs, but fake"))
(define fake-network
  (with-vat fake-network-vat
    (spawn ^fake-network)))

(define relay-server-vat
  (spawn-vat #:name "relay server"))

(define* (spawn-relay-admin #:rest args)
  (define relay-server-fake-netlayer
    (spawn-fake-netlayer fake-network "relay-server-fake"))
  (define relay-mycapn
    (spawn-mycapn relay-server-fake-netlayer))
  (define-values (warden incanter)
    (spawn-warding-pair))
  (values incanter
          (apply spawn ^relay-admin warden relay-mycapn 'fake 'fake args)))

(define-values (incanter relay-admin)
  (call-with-vat relay-server-vat spawn-relay-admin))

;; Create a user on the relay
(define alice-account-activate-sref-vow
  (with-vat relay-server-vat
    (<- incanter relay-admin 'register-account "alice")))
(define alice-account-activate-sref
  (match (resolve-vow-and-return-result
          relay-server-vat
          (lambda () alice-account-activate-sref-vow))
    (#('ok sref) sref)))

(test-equal "Relay admin get-accounts lists one account"
  #(ok ("alice"))
  (resolve-vow-and-return-result
   relay-server-vat
   (lambda ()
     (let-on ((accounts (<- incanter relay-admin 'get-accounts)))
       (hashmap-fold
        (lambda (label _ prev)
          (cons label prev))
        '()
        accounts)))))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

;; Setup alice's end
(define alice-vat
  (spawn-vat #:name "alice"))
(define alice-relay-netlayer-vow
  (with-vat alice-vat
    (define netlayer (spawn-fake-netlayer fake-network "alice"))
    (fetch-and-spawn-relay-netlayer netlayer (spawn-mycapn netlayer)
                                    alice-account-activate-sref)))
(define alice-relay-mycapn-vow
  (with-vat alice-vat
    (on alice-relay-netlayer-vow spawn-mycapn #:promise? #t)))
(define alice-greeter-sref-vow
  (with-vat alice-vat
    (<- alice-relay-mycapn-vow 'register (spawn ^greeter "Alice") 'relay)))

;; Setup bob's end
(define bob-account-activate-sref-vow
  (with-vat relay-server-vat
    (<- incanter relay-admin 'register-account "bob")))
(define bob-account-activate-sref
  (match (resolve-vow-and-return-result
          relay-server-vat
          (lambda () bob-account-activate-sref-vow))
    (#('ok sref) sref)))

(test-assert "Relay admin get-accounts lists both accounts"
  (match (resolve-vow-and-return-result
          relay-server-vat
          (lambda ()
            (let-on ((accounts (<- incanter relay-admin 'get-accounts)))
              (hashmap-fold
               (lambda (label _ prev)
                 (cons label prev))
               '()
               accounts))))
    (#(ok accounts)
     (equal? '("alice" "bob") (sort accounts string<=?)))
    (_ #f)))

(define bob-vat
  (spawn-vat #:name "bob"))
(define bob-relay-netlayer-vow
  (with-vat bob-vat
    (define netlayer (spawn-fake-netlayer fake-network "bob"))
    (fetch-and-spawn-relay-netlayer netlayer (spawn-mycapn netlayer)
                                    bob-account-activate-sref)))
(define bob-relay-mycapn-vow
  (with-vat bob-vat
    (on bob-relay-netlayer-vow spawn-mycapn #:promise? #t)))
(define alice-greeter-on-bob-vow
  (with-vat bob-vat
    (on alice-greeter-sref-vow
        (lambda (alice-greeter-sref)
          (<- bob-relay-mycapn-vow 'enliven alice-greeter-sref))
        #:promise? #t)))
(test-equal "Able to send message across after setup"
  #(ok "Hello Bob, my name is Alice!")
  (resolve-vow-and-return-result
   bob-vat
   (lambda ()
     (<- alice-greeter-on-bob-vow "Bob"))))

;; Check that account registering doesn't work on the "public" (un-incanted)
;; relay admin object. (we didn't turn that feature on)
(test-assert "Check ^relay-admin doesn't allow public registration when disabled"
  (let ((vat (spawn-vat)))
    (match (resolve-vow-and-return-result
            vat
            (lambda ()
              (define-values (incanter relay-admin)
                (spawn-relay-admin))
              (<- relay-admin 'register)))
      (#('ok _) #f)
      (#('err _) #t))))
(test-assert "Check ^relay-admin doesn't allow ephemeral accounts when disabled"
  (let ((vat (spawn-vat)))
    (match (resolve-vow-and-return-result
            vat
            (lambda ()
              (define-values (incanter relay-admin)
                (spawn-relay-admin))
              (<- relay-admin 'register-ephemeral)))
      (#('ok _) #f)
      (#('err _) #t))))

(test-assert "Check registering a public account when enabled"
  (let ((vat (spawn-vat)))
    (match (resolve-vow-and-return-result
            vat
            (lambda ()
              (define-values (incanter relay-admin)
                (spawn-relay-admin #:allow-registrations? #t))
              (<- relay-admin 'register)))
      (#('ok (? ocapn-sturdyref?)) #t)
      (#('err _) #f))))

(test-assert "Check registering a ephemeral account when enabled"
  (let ((vat (spawn-vat)))
    (match (resolve-vow-and-return-result
            vat
            (lambda ()
              (define-values (incanter relay-admin)
                (spawn-relay-admin #:allow-ephemeral-accounts? #t))
              (<- relay-admin 'register-ephemeral)))
      (#('ok (? ocapn-sturdyref?)) #t)
      (#('err _) #f))))

(test-assert "Check registering an account after re-configuring to disable registrations"
  (let ((vat (spawn-vat)))
    (match (resolve-vow-and-return-result
            vat
            (lambda ()
              (define-values (incanter relay-admin)
                (spawn-relay-admin #:allow-registrations? #t))
              (<-np incanter relay-admin 'configure #:allow-registrations? #f)
              (<- relay-admin 'register)))
      (#('ok (? ocapn-sturdyref?)) #f)
      (#('err _) #t))))

(test-assert "Check registering an ephemeral account after re-configuring to disable them"
  (let ((vat (spawn-vat)))
    (match (resolve-vow-and-return-result
            vat
            (lambda ()
              (define-values (incanter relay-admin)
                (spawn-relay-admin #:allow-ephemeral-accounts? #t))
              (<-np incanter relay-admin 'configure #:allow-ephemeral-accounts? #f)
              (<- relay-admin 'register-ephemeral)))
      (#('ok (? ocapn-sturdyref?)) #f)
      (#('err _) #t))))

;; TODO: Maybe move to (goblins utils hashmap)
(define (hashmap-null? hm)
  (call/ec
   (lambda (return)
     (hashmap-for-each
      (lambda _
        (return #f))
      hm)
     #t)))
(define (hashmap-length hm)
  (hashmap-fold
   (lambda (key val count)
     (1+ count))
   0
   hm))

(test-equal "Check ephemeral account sweep clears accounts"
  #(ok (1 0))
  (let ((vat (spawn-vat)))
    (resolve-vow-and-return-result
     vat
     (lambda ()
       (define-values (incanter relay-admin)
         (spawn-relay-admin #:allow-ephemeral-accounts? #t
                            #:ephemeral-account-lifespan 0.5))
       (let*-on ((account (<- relay-admin 'register-ephemeral))
                 (before-sweep-accounts ($ incanter relay-admin 'get-accounts))
                 (_ (timeout 0.6))
                 (after-sweep-accounts ($ incanter relay-admin 'get-accounts)))
         (list (hashmap-length before-sweep-accounts)
               (hashmap-length after-sweep-accounts)))))))

(test-assert "Check account sweep changes after reconfiguration"
  (let ((vat (spawn-vat)))
    (match (resolve-vow-and-return-result
            vat
            (lambda ()
              (define-values (incanter relay-admin)
                (spawn-relay-admin #:allow-ephemeral-accounts? #t))
              (define set-new-lifespan-vow
                (let*-on ((account (<- relay-admin 'register-ephemeral))
                          (accounts (<- incanter relay-admin 'get-accounts)))
                  (when (hashmap-null? accounts)
                    (error "Should be an ephemeral account"))
                  ;; Set the lifespan of ephemeral accounts to 500ms
                  ($ incanter relay-admin 'configure #:ephemeral-account-lifespan 0.5)
                  #t))
              (let*-on ((_ set-new-lifespan-vow)
                        (_ (timeout 0.6))
                        (accounts (<- incanter relay-admin 'get-accounts)))
                accounts)))
      (#('ok (? hashmap-null?)) #t)
      (_ #f))))

(test-equal "Check all ephemeral accounts are removed after disabled"
  #(ok 0)
  (let ((vat (spawn-vat)))
    (resolve-vow-and-return-result
     vat
     (lambda ()
       (define-values (incanter relay-admin)
         (spawn-relay-admin #:allow-ephemeral-accounts? #t))
       (let-on ((account (<- relay-admin 'register-ephemeral)))
         (when (hashmap-null? ($ incanter relay-admin 'get-accounts))
           (error "There should be an ephemeral account"))
         (<-np incanter relay-admin 'configure #:allow-ephemeral-accounts? #f)
         (on (<- incanter relay-admin 'get-accounts)
             hashmap-length
             #:promise? #t))))))

;; Check aurie, sometimes issues only show themselves after multiple restores.
;; Lets persist, restore two times and then check all works.
(define persistence-registry-vat (spawn-vat))
(define persistence-registry
  (with-vat persistence-registry-vat
    (spawn ^persistence-registry)))
(define network-store (make-memory-store))
(define-values (network-vat network)
  (spawn-persistent-vat
   fake-netlayer-env
   (lambda () (spawn ^fake-network))
   network-store
   #:persistence-registry persistence-registry))

(define server-store (make-memory-store))
(define server-env
  (persistence-env-compose relay-utils-env fake-netlayer-env captp-env))
(define-values (server-vat incanter relay-admin)
  (spawn-persistent-vat
   server-env
   (lambda ()
     (define netlayer (spawn-fake-netlayer network "relay-server"))
     (define mycapn (spawn-mycapn netlayer))
     (define-values (warden incanter)
       (spawn-warding-pair))
     (values incanter
             (spawn ^relay-admin warden mycapn 'fake 'fake
                    #:allow-registrations? #t
                    #:allow-ephemeral-accounts? #t)))
   server-store
   #:persistence-registry persistence-registry))

;; Then setup several kinds of accounts
(define admin-made-account
  (match (resolve-vow-and-return-result
          server-vat
          (lambda ()
            (<- incanter relay-admin 'register-account "Alisha")))
    (#('ok (? ocapn-sturdyref? sref)) sref)))
(define user-made-account
  (match (resolve-vow-and-return-result
          server-vat
          (lambda ()
            (<- relay-admin 'register)))
    (#('ok (? ocapn-sturdyref? sref)) sref)))
;; These should **not** suvive restores as they are *ephemeral*.
(define ephemeral-account
  (match (resolve-vow-and-return-result
          server-vat
          (lambda ()
            (<- relay-admin 'register-ephemeral)))
    (#('ok (? ocapn-sturdyref? sref)) sref)))

(vat-halt! server-vat)
(vat-halt! network-vat)
(define persistence-registry1
  (with-vat persistence-registry-vat
    (spawn ^persistence-registry)))
(define-values (network-vat1 network1)
  (spawn-persistent-vat
   fake-netlayer-env
   (lambda () (spawn ^fake-network))
   network-store
   #:persistence-registry persistence-registry1))

(define-values (server-vat1 incanter1 relay-admin1)
  (spawn-persistent-vat
   server-env
   (lambda ()
     (error "Should be restoring from the server store"))
   server-store
   #:persistence-registry persistence-registry1))

;; Then save and restore again, just to be doubley sure everything is working.
(vat-halt! server-vat1)
(define persistence-registry2
  (with-vat persistence-registry-vat
    (spawn ^persistence-registry)))
(define-values (network-vat2 network2)
  (spawn-persistent-vat
   fake-netlayer-env
   (lambda () (spawn ^fake-network))
   network-store
   #:persistence-registry persistence-registry2))
(define-values (server-vat2 incanter2 relay-admin2)
  (spawn-persistent-vat
   server-env
   (lambda ()
     (error "Should be restoring from the server store"))
   server-store
   #:persistence-registry persistence-registry2))

(define client-vat (spawn-vat))
(define client-netlayer
  (with-vat client-vat
    (spawn-fake-netlayer network2 "client-netlayer")))
(define client-mycapn
  (with-vat client-vat
    (spawn-mycapn client-netlayer)))

(test-assert "Can setup relay-netlayer from admin made account after restore"
  (match (resolve-vow-and-return-result
          client-vat
          (lambda ()
            (fetch-and-spawn-relay-netlayer client-netlayer client-mycapn
                                            admin-made-account)))
    (#('ok (? live-refr?)) #t)))

(test-assert "Can setup relay-netlayer from user made account after restore"
  (match (resolve-vow-and-return-result
          client-vat
          (lambda ()
            (fetch-and-spawn-relay-netlayer client-netlayer client-mycapn
                                            user-made-account)))
    (#('ok (? live-refr?)) #t)))

(test-assert "Check ephemeral accounts do not suvive persistence"
  (match (resolve-vow-and-return-result
          client-vat
          (lambda ()
            (fetch-and-spawn-relay-netlayer client-netlayer client-mycapn
                                            ephemeral-account)))
    (#('err _) #t)))

(test-assert "Check both accounts enumerate after restoration"
  (match (resolve-vow-and-return-result
          server-vat2
          (lambda ()
            (let-on ((accounts (<- incanter2 relay-admin2 'get-accounts)))
              (hashmap-length accounts))))
    (#('ok 2) #t)))

(test-assert "Admin can still register accounts after restoration"
  (match (resolve-vow-and-return-result
          server-vat2
          (lambda ()
            (<- incanter2 relay-admin2 'register-account "Another account")))
    (#('ok (? ocapn-sturdyref?)) #t)))

(test-assert "Users can still register accounts after restoration"
  (match (resolve-vow-and-return-result
          server-vat2
          (lambda ()
            (<- relay-admin2 'register)))
    (#('ok (? ocapn-sturdyref?)) #t)))

(test-assert "Users can still register ephemeral accounts after restoration"
  (match (resolve-vow-and-return-result
          server-vat2
          (lambda ()
            (<- relay-admin2 'register-ephemeral)))
    (#('ok (? ocapn-sturdyref?)) #t)))

(test-end "test-relay-utils")
