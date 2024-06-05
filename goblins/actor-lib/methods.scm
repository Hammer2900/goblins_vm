;;; Copyright 2019-2021 Christine Lemmer-Webber
;;; Copyright 2023, 2024 Juliana Sims
;;; Copyright 2024 David Thompson
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

(define-module (goblins actor-lib methods)
  #:use-module ((goblins) #:select ($ live-refr?))
  #:export (methods extend-methods))

(define (no-such-method method)
  ;; We curry like this so we can take advantage of case-lambda for making
  ;; most common uses of methods highly performant.
  (lambda args
    (error "No such method" method args)))

(define-syntax-rule (methods* fallback ((name arg ...) body body* ...) ...)
  (let ((lookup (lambda (method)
                  (case method
                    ((name) (lambda* (arg ...) body body* ...))
                    ...
                    (else (fallback method))))))
    (case-lambda
      (() (error "Method required"))
      ;; Optimize 0-3 arg cases to avoid consing args list.
      ((method) ((lookup method)))
      ((method arg1) ((lookup method) arg1))
      ((method arg1 arg2) ((lookup method) arg1 arg2))
      ((method arg1 arg2 arg3) ((lookup method) arg1 arg2 arg3))
      ;; Cons arg list for general case.
      ((method . args) (apply (lookup method) args)))))

(define-syntax-rule (methods method-defns ...)
  ;; Describe an actor's methods.
  ;;
  ;; A Method-Defn has the form `((name args ...) expr)' where 'NAME is the
  ;; symbol upon which the method dispatches, ARGS are the arguments it
  ;; accepts, and EXPR is the expression evaluated when the method is called.
  ;;
  ;; Type: (Method-Defn ...) -> Methods
  (methods* no-such-method method-defns ...))

(define-syntax-rule (extend-methods extends method-defns ...)
  ;; Extend EXTENDS with METHOD-DEFNS.
  ;;
  ;; A METHOD-DEFN has the form `((name args...) expr)' where 'NAME is the symbol
  ;; upon which the method dispatches, ARGS are the arguments it accepts, and
  ;; EXPR is the expression evaluated when the method is called.
  ;;
  ;; Type: (U Actor Procedure) (Method-Defn ...) -> Methods

  ;; We curry extends here so that we can use case-lambda and make the most
  ;; common uses of methods performant while still supporting extension.
  ;; We need to capture extendable in an outer let otherwise it re-runs the
  ;; extendable code every time the procedure returned by methods is invoked.
  (let ((extendable (cond
                     ((procedure? extends)
                      (lambda (method)
                        extends))
                     ((live-refr? extends)
                      (lambda (method)
                        (lambda args
                          (apply $ extends method args))))
                     (else no-such-method))))
    (methods* extendable method-defns ...)))

;; performance testing

;; (define enum
;;   (methods
;;    ((average a b c)
;;     (/ (+ a b c) 3))
;;    ((foo) 1)
;;    ((bar) 2)
;;    ((baz) 3)
;;    ((zort) 4)
;;    ((narf) 5)
;;    ((bloop) 6)))

;; ((@ (statprof) statprof)
;;  (lambda () (enum 'foo) (enum 'average 5 10 3))
;;  #:loop 50000000)
