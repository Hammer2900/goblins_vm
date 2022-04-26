;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.
;; Copyright (C) 2019-2021 Christine Lemmer-Webber

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

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (goblins ocapn define-recordable)
  #:use-module (goblins contrib syrup)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (define-recordable))

;; Borrowed and modified from the module (language cps)
;; So, LGPLv3+ on this one I guess

;;; This defines a record that can also be serialized into a datastructure.
;;; Also defines marshall::<name> and unmarshall::<name> procedures 

(define-syntax-rule (define-recordable record-name record-constructor
                      record?
                      marshall unmarshall
                      (field field-accessor) ...)
  (begin
    (define-record-type record-name
      (record-constructor field ...)
      record?
      (field field-accessor) ...)

    (define marshall
      (cons (match-lambda
              (($ record-name field ...)
               #t)
              (_ #f))
            (match-lambda
              (($ record-name field ...)
               (make-syrec* (quote record-name) field ...)))))

    (define unmarshall
      (cons (lambda (label)
              (eq? label (quote record-name)))
            record-constructor))))

