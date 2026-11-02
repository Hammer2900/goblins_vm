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
(define-module (goblins cli)
  #:use-module (goblins cli relay-server)
  #:use-module (ice-9 match)
  #:export (goblins-main))

(define (display-help)
  (format #t "Goblins commands:\n")
  (format #t "  - relay\n")
  (format #t "You can use \"goblins help <command>\" to display help for a specific command\n"))

(define (goblins-main arguments)
  (match arguments
    ((_ "relay" relay-args ...)
     (relay-server-command relay-args))
    (_ (display-help))))
