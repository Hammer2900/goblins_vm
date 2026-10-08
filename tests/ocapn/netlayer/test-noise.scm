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
(use-modules (ice-9 match)
             (rnrs bytevectors)
             (goblins)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer noise)
             (goblins ocapn netlayer fake)
             (goblins actor-lib cell)
             (goblins persistence-store memory)
             (tests utils)
             (srfi srfi-64))

(test-begin "test-noise-netlayer")

;; Setup the fake netlayer
(define test-vat (spawn-vat #:name "test-vat"))
(define test-network (with-vat test-vat (spawn ^fake-network)))

(define a-vat (spawn-vat #:name "a-vat"))
(define a-fake-netlayer (with-vat a-vat (spawn-fake-netlayer test-network "a")))
(define a-noise-netlayer
  (with-vat a-vat (spawn ^noise-netlayer a-fake-netlayer)))
(define a-mycapn (with-vat a-vat (spawn-mycapn a-noise-netlayer)))
(define a-cell (with-vat a-vat (spawn ^cell 'apple)))
(define a-cell-sref
  (match (resolve-vow-and-return-result
          a-vat (lambda () ($ a-mycapn 'register a-cell 'noise)))
    (#(ok sref) sref)
    (_ (error "Could not generate sturdyref"))))

(define b-vat (spawn-vat #:name "b-vat"))
(define b-fake-netlayer (with-vat b-vat (spawn-fake-netlayer test-network "b")))
(define b-noise-netlayer
  (with-vat b-vat (spawn ^noise-netlayer b-fake-netlayer)))
(define b-mycapn (with-vat b-vat (spawn-mycapn b-noise-netlayer)))
(define remote-cell
  (with-vat b-vat
    (<- b-mycapn 'enliven a-cell-sref)))
(test-assert "Check we can enliven a remote refr for the noise netlayer"
  (match (resolve-vow-and-return-result b-vat (lambda () remote-cell))
    (#(ok (? remote-object-refr?)) #t)
    (_ #f)))

(test-equal "Check we can message the remote refr using the noise netlayer"
  #(ok apple)
  (resolve-vow-and-return-result b-vat (lambda () (<- remote-cell))))

(test-assert "Check a noise sturdyref can be encoded to a string"
  (match (resolve-vow-and-return-result a-vat (lambda () a-cell-sref))
    (#(ok cell-sref) (string? (ocapn-id->string cell-sref)))
    (_ #f)))

;; Noise messages cannot exceed 65519 bytes long. The noise netlayer chunks
;; messages over this size, lets check chunking. We'll set the value of our cell
;; to be a really large bytevector, and then read it back which will pass it
;; over OCapN twice!
;; Just for robustness sake, let's populate the bv with different data.
(define our-long-bv-length 555912)
(define our-long-bv (make-bytevector our-long-bv-length))
(let lp ((index 0))
  (let ((new-index (+ index 8)))
    ;; The value doesn't matter, it's mostly so it's not all the same value.
    (bytevector-fill! our-long-bv (modulo index 255) index new-index)
    (when (< new-index our-long-bv-length)
      (lp new-index))))

(test-equal "Check noise supports sending a large value"
  (vector 'ok our-long-bv)
  (resolve-vow-and-return-result
   b-vat
   (lambda ()
     (<-np remote-cell our-long-bv)
     (<- remote-cell))))

;; Test aurie + noise
;; The main thing we're testing here is persistent sturdyrefs by the noise
;; netlayer persisting both the peer static key and the noise connector's sref.
(define store (make-memory-store))
(define-values (aurie-vat aurie-network aurie-mycapn)
  (spawn-persistent-vat
   (persistence-env-compose noise-netlayer-env fake-netlayer-env cell-env)
   (lambda ()
     (define fake-network (spawn ^fake-network))
     (define fake-netlayer (spawn-fake-netlayer fake-network "alice"))
     (define noise-netlayer (spawn ^noise-netlayer fake-netlayer))
     (values fake-network  (spawn-mycapn noise-netlayer)))
   store
   #:name "Alice vat"))

(define alice-cell-sref
  (match (resolve-vow-and-return-result
          aurie-vat
          (lambda ()
            (<- aurie-mycapn 'register (spawn ^cell 'apple) 'noise)))
    (#(ok sref) sref)
    (_ (error "Couldn't generate sturdyref for the cell."))))

;; Okay now stop the aurie vat so we can restore from the store and see if we
;; can still enliven the cell.
(vat-halt! aurie-vat)
(define-values (aurie-vat* aurie-network* aurie-mycapn*)
  (spawn-persistent-vat
   (persistence-env-compose noise-netlayer-env fake-netlayer-env cell-env)
   (lambda () (error "Should be restoring it from the store"))
   store
   #:name "Restored Alice vat"))


;; Now make another peer so we can try and enliven and access our cell.
(test-equal "Check we can enliven and access cell on restored noise netlayer"
  #(ok apple)
  (let* ((bob-vat (spawn-vat #:name "bob"))
         (bob-fake (with-vat bob-vat (spawn-fake-netlayer aurie-network* "bob")))
         (bob-noise (with-vat bob-vat (spawn ^noise-netlayer bob-fake)))
         (bob-mycapn (with-vat bob-vat (spawn-mycapn bob-noise))))
    (resolve-vow-and-return-result
     bob-vat
     (lambda ()
       (<- (<- bob-mycapn 'enliven alice-cell-sref))))))

(test-end "test-noise-netlayer")
