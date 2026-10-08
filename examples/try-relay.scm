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

(use-modules (goblins)
             (goblins actor-lib cell)
             (goblins actor-lib facet)
             (goblins actor-lib methods)
             (goblins actor-lib on)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer relay)
             (goblins ocapn netlayer tcp-tls)
             (goblins ocapn netlayer relay-utils))

(define (make-relay-server name)
  (define vat (spawn-vat #:name (format #f "Relay Server: ~a" name)))
  (define netlayer (with-vat vat (spawn ^tcp-tls-netlayer "localhost")))
  (define mycapn (with-vat vat (spawn-mycapn netlayer)))
  (define relay-admin
    (with-vat vat
      (spawn ^relay-admin
             (facet mycapn 'enliven)
             (spawn (lambda (_bcom)
                      (methods
                       ((register obj) (<- mycapn 'register obj 'tcp-tls))))))))
  (values vat relay-admin))

;; -----------
;; Setup Alice
;; -----------
(define-values (a-relay-server-vat a-relay-server-admin)
  (make-relay-server "a"))
(define alice-account-sref-vow
  (with-vat a-relay-server-vat
    ($ a-relay-server-admin 'add-account "alice")))

(define alice-vat (spawn-vat #:name "Client: Alice"))
(define alice-netlayer
  (with-vat alice-vat
    (spawn ^tcp-tls-netlayer "localhost")))
(define alice-relay-netlayer-vow
  (with-vat alice-vat
    (let-on ((alice-account-sref alice-account-sref-vow))
      (fetch-and-spawn-relay-netlayer alice-account-sref
                                      #:netlayer alice-netlayer))))
(define alice-mycapn
  (with-vat alice-vat
    (let-on ((alice-relay-netlayer alice-relay-netlayer-vow))
      (spawn-mycapn alice-relay-netlayer))))

;; Setup a cell
(define alice-lunchbox
  (with-vat alice-vat (spawn ^cell 'chickpea)))
(define alice-lunchbox-sref-vow
  (with-vat alice-vat ($ alice-mycapn 'register alice-lunchbox 'relay)))

(with-vat alice-vat
  (let-on ((alice-lunchbox-sref alice-lunchbox-sref-vow))
    (pk 'alice-lunchbox-sref (ocapn-id->string alice-lunchbox-sref))))

;; ---------
;; Setup Bob
;; ---------
(define-values (b-relay-server-vat b-relay-server-admin)
  (make-relay-server "b"))
(define bob-account-sref-vow
  (with-vat b-relay-server-vat
    ($ b-relay-server-admin 'add-account "bob")))
(define bob-vat (spawn-vat #:name "Client: Bob"))
(define bob-netlayer
  (with-vat bob-vat
    (spawn ^tcp-tls-netlayer "localhost")))
(define bob-relay-netlayer-vow
  (with-vat bob-vat
    (let-on ((bob-account-sref bob-account-sref-vow))
      (fetch-and-spawn-relay-netlayer bob-account-sref
                                      #:netlayer bob-netlayer))))
(define bob-mycapn
  (with-vat bob-vat
    (let-on ((bob-relay-netlayer bob-relay-netlayer-vow))
      (spawn-mycapn bob-relay-netlayer))))

(with-vat bob-vat
  (define remote-alice-lunchbox
    (<- bob-mycapn 'enliven alice-lunchbox-sref-vow))
  (let-on ((contents (<- remote-alice-lunchbox)))
    (pk 'lunchbox-contents contents)))

(sleep 5)

