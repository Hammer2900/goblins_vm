;;; Copyright 2026 David Thompson
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

(define-library (goblins persistence-store indexed-db)
  (export make-indexed-db-store)
  (cond-expand
   (guile
    (import (scheme base)))
   (hoot
    (import (fibers promises)
            (guile)
            (goblins core-types)
            (goblins actor-lib methods)
            (goblins contrib syrup)
            (hoot ffi)
            (hoot typed-arrays)
            (ice-9 match)
            (rnrs bytevectors))))

  (begin
    (cond-expand
     (guile
      (define (make-indexed-db-store name)
        (error "IndexedDB not available on Guile VM")))
     (hoot
      ;; string Function -> Promise<IDBDatabase>
      (define-foreign %db-open
        "indexedDB" "dbOpen"
        (ref string) (ref extern) -> (ref extern))
      ;; IDBDatabase string -> IDBObjectStore
      (define-foreign db-create-object-store!
        "indexedDB" "dbCreateObjectStore"
        (ref extern) (ref string) -> (ref extern))
      ;; IDBDatabase string string -> IDBTransaction
      (define-foreign db-transaction
        "indexedDB" "dbTransaction"
        (ref extern) (ref string) (ref string) -> (ref extern))
      ;; IDBDatabase string string string -> IDBTransaction
      (define-foreign db-transaction-2
        "indexedDB" "dbTransaction2"
        (ref extern) (ref string) (ref string) (ref string) -> (ref extern))
      ;; IDBTransaction string -> IDBObjectStore
      (define-foreign transaction-object-store
        "indexedDB" "transactionObjectStore"
        (ref extern) (ref string) -> (ref extern))
      ;; IDBTransaction -> Promise
      (define-foreign %transaction-commit!
        "indexedDB" "transactionCommit"
        (ref extern) -> (ref extern))
      ;; IDBObjectStore -> Promise<Number>
      (define-foreign object-store-count
        "indexedDB" "objectStoreCount"
        (ref extern) -> (ref extern))
      ;; IDBObjectStore Function -> Promise
      (define-foreign %object-store-for-each
        "indexedDB" "objectStoreForEach"
        (ref extern) (ref extern) -> (ref extern))
      ;; We're using integers as object keys, but we use an f64 rather
      ;; than i32 here so we can use the full 53-bit range of JS
      ;; integers before overflowing to BigInts.
      ;;
      ;; IDBObjectStore f64 -> IDBRequest
      (define-foreign object-store-get
        "indexedDB" "objectStoreGet"
        (ref extern) f64 -> (ref extern))
      ;; IDBObjectStore f64 Uint8Array -> IDBRequest
      (define-foreign object-store-put
        "indexedDB" "objectStorePut"
        (ref extern) f64 (ref extern) -> (ref extern))

      (define (db-open name upgrade)
        (await (%db-open name (procedure->external upgrade))))

      (define (transaction-commit! transaction)
        (await (%transaction-commit! transaction)))

      (define (object-store-for-each proc store)
        (await (%object-store-for-each store (procedure->external proc))))

      (define (object-store-fold proc init store)
        (define memo init)
        (object-store-for-each (lambda (k v)
                                 (set! memo (proc k v memo)))
                               store)
        memo)

      (define (db-count db name)
        (let* ((transaction (db-transaction db name "readonly"))
               (store (transaction-object-store transaction name)))
          (await (object-store-count store))))

      (define (db-get db name key)
        (let* ((transaction (db-transaction db name "readonly"))
               (store (transaction-object-store transaction name)))
          (await (object-store-get store key))))

      (define (db-fold proc init db name)
        (let* ((transaction (db-transaction db name "readonly"))
               (store (transaction-object-store transaction name)))
          (object-store-fold proc init store)))

      ;; TODO: Upstream to (hoot typed-arrays)
      (define (uint8array->bytevector array)
        (let* ((len (uint8array-length array))
               (bv (make-bytevector len)))
          (let lp ((i 0))
            (when (< i len)
              (bytevector-u8-set! bv i (uint8array-ref array i))
              (lp (1+ i))))
          bv))

      (define (syrup-encode* x)
        (bytevector->uint8array (syrup-encode x)))

      (define (syrup-decode* blob)
        (syrup-decode (uint8array->bytevector blob)))

      (define (make-indexed-db-store name)
        (define (upgrade db)
          ;; For vat metadata (id, version).
          (db-create-object-store! db "metadata")
          ;; For actor portrait data.
          (db-create-object-store! db "actors"))
        (define db (db-open name upgrade))
        (define (save-portraits store portraits)
          (hash-for-each (lambda (slot portrait)
                           (let ((blob (syrup-encode* portrait)))
                             (object-store-put store slot blob)))
                         portraits))
        ;; Using a magic number as the key for the vat metadata table
        ;; so that we don't have to support another key type for
        ;; objectStore.get().
        (define %vat-key 1)
        (define reader
          (methods
           ((graph-and-slots)
            (if (zero? (db-count db "actors"))
                (values #f #f #f #f)
                (match (syrup-decode* (db-get db "metadata" %vat-key))
                  ((vat-id version)
                   (let* ((portraits (make-hash-table))
                          (slots (db-fold
                                  (lambda (slot blob prev)
                                    ;; All JS numbers are floats, so we
                                    ;; have to convert to an integer
                                    ;; here.
                                    (let* ((slot (inexact->exact slot))
                                           (portrait (syrup-decode* blob)))
                                      (hashv-set! portraits slot portrait)
                                      (cons slot prev)))
                                  '() db "actors")))
                     (values vat-id version portraits slots))))))
           ((object-portrait slot)
            (syrup-decode* (db-get db "actors" slot)))))
        (define writer
          (methods
           ((save-graph vat-id version portraits slots)
            (let* ((transaction (db-transaction-2 db "metadata" "actors"
                                                  "readwrite"))
                   (metadata (transaction-object-store transaction "metadata"))
                   (actors (transaction-object-store transaction "actors")))
              (object-store-put metadata %vat-key
                                (syrup-encode* (list vat-id version)))
              (save-portraits actors portraits)
              (transaction-commit! transaction)))
           ((save-delta portraits)
            (let* ((transaction (db-transaction db "actors" "readwrite"))
                   (actors (transaction-object-store transaction "actors")))
              (save-portraits actors portraits)
              (transaction-commit! transaction)))))
        (make-persistence-store reader writer))))))
