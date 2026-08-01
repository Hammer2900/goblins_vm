;;; Copyright 2019-2021 Christine Lemmer-Webber
;;; Copyright 2023 Juliana Sims
;;; Copyright 2026 David Thompson
;;; Copyright 2026 Nikolaos Chatzikonstantinou
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
  #:use-module (ice-9 match)
  #:use-module (goblins core)
  #:use-module (goblins actor-lib opportunistic)
  #:export (methods extend-methods))

(define-syntax dispatch-table
  (syntax-rules ()
    ((_ name fallback)
     ;; The first argument to the fallback is the requested method
     ;; name that was not found.
     (lambda args (apply fallback name args)))
    ((_ name ((method-name . method-args) method-body ...) . rest)
     (if (eq? name 'method-name)
         (lambda* method-args method-body ...)
         (dispatch-table name . rest)))
    ((_ name (method-name method-expr) . rest)
     (if (eq? name 'method-name)
         method-expr
         (dispatch-table name . rest)))))

(define-syntax-rule (methods* fallback method-defn ...)
  (lambda (name . args)
    (apply (dispatch-table name method-defn ... fallback) args)))

(define (no-such-method method . args)
  (error "No such method" method args))

(define-syntax-rule (methods method-defns ...)
  ;;; Describe a collection of methods that an actor accepts.
  ;;;
  ;;; A METHOD-DEFN has one of two forms:
  ;;; 1. `((name args...) body ...)' where 'NAME is the symbol
  ;;;    upon which the method dispatches, ARGS are the arguments
  ;;;    the dispatch method accepts, and BODY is the expression
  ;;;    evaluated when the method is called, i.e. its body.
  ;;; 2. `(name proc)' where 'NAME is the symbol upon which the method
  ;;;    dispatches, PROC is the dispatch method.
  ;;;
  ;;; Type: (Method-Defn ...) -> Methods
  (methods* no-such-method method-defns ...))

(define (extend-actor extends-actor)
  (lambda (method . args)
    (define $/<-
      (select-$/<- extends-actor))
    (apply $/<- extends-actor method args)))

(define-syntax-rule (extend-methods extends method-defns ...)
  ;;; Extend EXTENDS with METHOD-DEFNS.
  ;;;
  ;;; The METHOD-DEFN is the same as in the comments of the METHODS macro.
  ;;;
  ;;; Type: (U Actor Procedure) (Method-Defn ...) -> Methods

  ;; We need to capture extended in a an outer let here otherwise
  ;; it re-runs the extended code every time the procedure returned by
  ;; methods is invoked
  (let ((extended (match extends
                    ;; we extend procedures as-is
                    ((? procedure?) extends)
                    ;; but wrap actors in procedure that calls them
                    ((? live-refr?)
                     (extend-actor extends))
                    (#f no-such-method))))
    (methods* extended method-defns ...)))
