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
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer tcp-tls)
             (goblins ocapn netlayer noise)
             (goblins actor-lib cell))

(define vat-a (spawn-vat #:name "a"))
(define vat-b (spawn-vat #:name "b"))

(define a-mycapn
  (with-vat vat-a
    (define tcp-tls-netlayer
      (spawn ^tcp-tls-netlayer "127.0.0.1"))
    (define noise-netlayer
      (spawn ^noise-netlayer tcp-tls-netlayer))
    (spawn-mycapn noise-netlayer)))


(define a-cell (with-vat vat-a (spawn ^cell 'apple)))
(define a-cell-sref
  (with-vat vat-a
    (on (<- a-mycapn 'register a-cell 'noise)
        (lambda (sref)
          (format #t "sturdyref: ~a" (ocapn-id->string sref))
          sref)
        #:promise? #t)))

(define b-mycapn
  (with-vat vat-b
    (define tcp-tls-netlayer
      (spawn ^tcp-tls-netlayer "127.0.0.1"))
    (define noise-netlayer
      (spawn ^noise-netlayer tcp-tls-netlayer))
    (spawn-mycapn noise-netlayer)))
(define remote-cell-vow
  (with-vat vat-b (<- b-mycapn 'enliven a-cell-sref)))
(with-vat vat-b
  (on remote-cell-vow
      (lambda (remote-cell)
        (format #t "remote-cell: ~a" remote-cell))))
(with-vat vat-b
  (on (<- remote-cell-vow)
      (lambda (contents)
        (format #t "Cell contains: ~a" contents))))
(sleep 5)