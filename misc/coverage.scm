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

(use-modules (system vm coverage)
             (system vm vm)
             (srfi srfi-11)
             (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 match))

;; Backported from guile (modules/system/vm/coverage.scm), this code
;; is LGPL v3. This is backported to get the modules keyword which
;; provided in bug#54911.
(define* (backport/coverage-data->lcov data port #:key (modules #f))
  "Traverse code coverage information DATA, as obtained with
`with-code-coverage', and write coverage information in the LCOV format to PORT.
The report will include all the modules loaded at the time coverage data was
gathered, even if their code was not executed."

  ;; Output per-file coverage data.
  (format port "TN:~%")
  (define source-files
    (filter
     (lambda (file)
       (or (not modules) (member file modules)))
     (instrumented-source-files data)))

  (for-each (lambda (file)
              (let ((path (search-path %load-path file)))
                (if (string? path)
                    (begin
                      (format port "SF:~A~%" path)
                      #;
                      (for-each dump-function procs)
                      (for-each (lambda (line+count)
                                  (let ((line  (car line+count))
                                        (count (cdr line+count)))
                                    (format port "DA:~A,~A~%"
                                            (+ 1 line) count)))
                                (line-execution-counts data file))
                      (let-values (((instr exec)
                                    (instrumented/executed-lines data file)))
                        (format port "LH: ~A~%" exec)
                        (format port "LF: ~A~%" instr))
                      (format port "end_of_record~%"))
                    (begin
                      (format (current-error-port)
                              "skipping source file: ~a~%"
                              file)))))
            source-files))

(define (run-coverage arguments)
  "Run coverage check and write the result out into the lcov.info file"
  (define src-files
    (assq-ref arguments 'src))

  ;; No idea why but it seems that the REPL tests just hang.
  ;; TODO: FIX ME
  (define tests-without-repl
    (delete "tests/test-repl.scm" (assq-ref arguments 'tests)))

  (define-values (data result)
    (with-code-coverage
     (lambda ()
       (test-with-runner
           (test-runner-null)
         (map load-from-path tests-without-repl))
       ;; Load all files wtihin goblins so that coverage knows about them.
       (map load-from-path src-files))))

  (let ((port (open-output-file "lcov.info")))
    (backport/coverage-data->lcov data port #:modules src-files)))

;; Ideally we'd use getopt-long but unfortunately it doesn't seem to work for a
;; single flag taking many arguments e.g.
;; --src <src-file ...> --tests <test-file ..>
(define (parse-args args)
  "Collects each option given based on the command line flag provided"
  (define (is-option? argument)
    (string-prefix? "--" argument))
  (define (parse args current-option result)
    (match args
      [() result]
      [((? is-option? option) rest ...)
       (parse rest
              (string->symbol (substring option 2))
              result)]
      [(argument rest ...)
       (parse rest
              current-option
              (if current-option
                  (assq-set! result
                             current-option
                             (cons argument (or (assq-ref result current-option) '())))
                  result))]))
  (parse args #f '()))

(define arguments
  (parse-args (command-line)))

(if (and (assq-ref arguments 'tests)
         (assq-ref arguments 'src))
    (run-coverage arguments)
    (format (current-error-port)
            "Usage: ~a --src <src-file ...> --tests <test-file ...>"
            (car (command-line))))
