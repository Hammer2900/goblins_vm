;;; Copyright 2023 Christine Lemmer-Webber
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

(define-module (goblins utils list-ref)
  #:export (list-ref))

(define (list-ref lst i)
  (unless (and (integer? i) (>= i 0))
    (error "`i' must be an integer 0 or greater"))
  (let lp ((n 0)
           (lst lst))
    (cond
     ((null? lst)
      (error "`i' exceeds length of list"))
     ((= n i)
      (car lst))
     (else
      (lp (1+ n) (cdr lst))))))


