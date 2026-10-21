(define-module (tests contrib test-syrup-internals)
  #:use-module (goblins contrib syrup)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)          ; tests
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match))

;; Pull in a bunch of internals from Syrup to test
(define-syntax-rule (snarf-syrup-internals id ...)
  (begin
    (define id (@@ (goblins contrib syrup) id)) ...))

(test-begin "test-syrup-internals")

;;; Netstring tests
;;; ===============

(snarf-syrup-internals write-netstring!)

(test-equal "netstring-encode works"
  "12:Hello world!"
  (bytevector->string
   (call-with-output-bytevector
    (lambda (port)
      (write-netstring! port (string->bytevector "Hello world!" "ISO-8859-1"))))
   "ISO-8859-1"))

;;; Byte utilities tests
;;; ====================

(snarf-syrup-internals bytes bytes<?)

(test-assert "same bytestring isn't less"
  (not (bytes<? (bytes "meep")
                (bytes "meep"))))
(test-assert "greater bytestring of same length isn't less"
  (not (bytes<? (bytes "meep")
                (bytes "beep"))))
(test-assert "lesser bytestring of same length is less"
  (bytes<? (bytes "beep")
           (bytes "meep")))
(test-assert "greater bytestring of same length isn't less 2"
  (not (bytes<? (bytes "meep")
                (bytes "meeb"))))
(test-assert "lesser bytestring of same length is less 2"
  (bytes<? (bytes "meeb")
           (bytes "meep")))
(test-assert "shorter bytestring is less"
  (bytes<? (bytes "meep")
           (bytes "meeple")))
(test-assert "longer bytestring is greater"
  (not (bytes<? (bytes "meeple")
                (bytes "meep"))))

(test-end "test-syrup-internals")
