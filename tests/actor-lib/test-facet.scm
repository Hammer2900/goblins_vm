;;; Copyright 2020-2022 Christine Lemmer-Webber
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

(define-module (tests actor-lib test-facet)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib facet)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-facet")

(define test-vat (spawn-vat))

(define (^wizard bcom)
  (match-lambda*
    [('magic-missile level)
     (format #f "Casts magic missile level ~a!"
             level)]
    [('flame-tongue level)
     (format #f "Casts flame tongue level ~a!"
             level)]
    [('world-ender level)
     (format #f "Casts world ender level ~a!"
             level)]))

(define all-powerful-wizard
  (test-vat (lambda () (spawn ^wizard))))

(define faceted-wizard
  (test-vat
   (lambda ()
     (spawn ^facet
	    all-powerful-wizard
	    'magic-missile
	    'flame-tongue
	    #:sync? #t))))

(test-equal
    "We can fire a magic missile with the faceted wizard"
  (test-vat (lambda () ($ faceted-wizard 'magic-missile 2)))
  "Casts magic missile level 2!")
(test-equal
    "We can fire a flame tongue with the faceted wizard"
    (test-vat (lambda () ($ faceted-wizard 'flame-tongue 3)))
    "Casts flame tongue level 3!")

(test-error
 "The facet prevents using a method it's not forwarding"
 #t
 (test-vat (lambda () ($ faceted-wizard 'world-ender 99))))

(test-end "test-facet")

