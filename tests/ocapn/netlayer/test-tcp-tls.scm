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
             (tests utils)
             (ice-9 match)
             (srfi srfi-64))

(test-begin "test-tcp-tls")

(test-assert "Can spawn tcp-tls netlayer (no provided cert/key)"
  (with-vat (spawn-vat)
    (live-refr? (spawn ^tcp-tls-netlayer "localhost"))))

(test-equal "Can communicate over the tcp-tls netlayer"
  #(ok hi)
  (let* ((vat-a (spawn-vat))
         (vat-b (spawn-vat))
         (mycapn-a (with-vat vat-a
                     (spawn-mycapn (spawn ^tcp-tls-netlayer "localhost"))))
         (mycapn-b (with-vat vat-b
                     (spawn-mycapn (spawn ^tcp-tls-netlayer "localhost"))))
         (greeter (with-vat vat-a
                    (define (^greeter bcom) (lambda () 'hi))
                    (spawn ^greeter)))
         (greeter-sref (with-vat vat-a
                         ($ mycapn-a 'register greeter 'tcp-tls))))
    (resolve-vow-and-return-result
     vat-b
     (lambda ()
       (<- (<- mycapn-b 'enliven greeter-sref))))))

(test-end "test-tcp-tls")
