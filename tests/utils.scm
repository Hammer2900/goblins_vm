;;; Copyright 2022 Jessica Tallon
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

(define-library (tests utils)
  (import (goblins core)
          (goblins vat)
          (guile)
          (fibers)
          (fibers channels)
          (fibers operations)
          (fibers timers)
          (ice-9 match))
  (cond-expand
   (hoot
    (import (only (fibers promises)
                  call-with-async-result)))
   (guile
    (import (srfi srfi-1)))) ;; we don't need this; we can't have empty clauses
  (export am-resolve-vow-and-return-result
          resolve-vow-and-return-result
          persist-and-restore
          am-churn! am-churn!*
          wrap-async-tests
          maybe-run-fibers)
  (begin
    (define-syntax-rule (maybe-run-fibers thunk args ...)
      (cond-expand
       (hoot (thunk))
       (guile (run-fibers thunk args ...))))

    (define* (am-resolve-vow-and-return-result am goblins-thunk #:key (timeout 2))
      (define vow (actormap-churn-run! am goblins-thunk))
      (maybe-run-fibers
       (lambda ()
         (let ((results-ch (make-channel)))
           (actormap-churn-run!
            am
            (lambda ()
              (on vow
                  (lambda args
                    (syscaller-free-fiber
                     (lambda ()
                       (put-message results-ch (apply vector 'ok args))))
                    'ok)
                  #:catch
                  (lambda err
                    (syscaller-free-fiber
                     (lambda ()
                       (put-message results-ch (vector 'err err))))
                    'err))))
           (perform-operation
            (choice-operation (wrap-operation (sleep-operation timeout)
                                              (lambda _
                                                (vector 'err '*timeout*)))
                              (get-operation results-ch)))))))

    (define* (resolve-vow-and-return-result vat goblins-thunk #:key (timeout 2))
      (define vow (call-with-vat vat goblins-thunk))
      (define results-ch (make-channel))
      (with-vat vat
        (on vow
            (lambda args
              (syscaller-free-fiber
               (lambda ()
                 (put-message results-ch (apply vector 'ok args))))
              'ok)
            #:catch
            (lambda err
              (syscaller-free-fiber
               (lambda ()
                 (put-message results-ch (vector 'err err))))
              'err)))

      (perform-operation
       (choice-operation (wrap-operation (sleep-operation timeout)
                                         (lambda _
                                           (vector 'err '*timeout*)))
                         (get-operation results-ch))))

    (define (persist-and-restore am env . roots)
      (define-values (portraits root-slots)
        (apply actormap-take-portrait am env roots))
      (define restored-am
        (make-actormap))
      (apply
       values
       restored-am
       (call-with-values
           (lambda ()
             (actormap-restore! restored-am env portraits root-slots))
         list)))

    (define-syntax-rule (wrap-async-tests body ...)
      (cond-expand
       (hoot
        (lambda (resolved rejected)
          (call-with-async-result
           resolved rejected
           (lambda ()
             body ...))))
       (guile
        (let ()
          body ...))))))
