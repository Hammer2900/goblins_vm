(define-module (tests ocapn netlayer test-testuds)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer testuds)
  #:use-module (tests utils)
  #:use-module (srfi srfi-64))

(test-begin "test-testuds-netlayer")

(define test-vat (spawn-vat #:name "test-vat"))

;; Tests for the ^testuds-netlayer
(define a-vat (spawn-vat #:name "a-vat"))
(define b-vat (spawn-vat #:name "b-vat"))
(define a-location (string->ocapn-id "ocapn://a.testuds"))
(define b-location (string->ocapn-id "ocapn://b.testuds"))

(define netlayers-dir (tmpnam))
(unless (file-exists? netlayers-dir)
  (mkdir netlayers-dir))

(define a-netlayer
  (with-vat a-vat
    (spawn ^testuds-netlayer netlayers-dir #:peer-id "a")))
(define b-netlayer
  (with-vat b-vat
    (spawn ^testuds-netlayer netlayers-dir #:peer-id "b")))

(define a-mycapn
  (with-vat a-vat (spawn-mycapn a-netlayer)))
(define b-mycapn
  (with-vat b-vat (spawn-mycapn b-netlayer)))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define alice
  (with-vat a-vat
    (spawn ^greeter "Alice")))
(define bob
  (with-vat b-vat
    (spawn ^greeter "Bob")))

(define alice-locator-sref
  (with-vat a-vat
    ($ a-mycapn 'register alice 'testuds)))
(define bob-locator-sref
  (with-vat b-vat
    ($ b-mycapn 'register bob 'testuds)))

(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (define bob-vow (<- a-mycapn 'enliven bob-locator-sref))
          (<- bob-vow "Arthur")))))
  (test-equal "Able to enliven a far sturdyref and using it from a->b"
    #(ok "Hello Arthur, my name is Bob!")
    result))

(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda ()
          (define alice-vow (<- b-mycapn 'enliven alice-locator-sref))
          (<- alice-vow "Ben")))))
  (test-equal "Able to enliven a far sturdyref and using it form b->a"
    #(ok "Hello Ben, my name is Alice!")
    result))

;; Test on-sever works in the testuds netlayer
;; FIXME: this is not implemented at the moment. But given how testuds is only to be used for testing,
;; it likely is not be a priority.
;;
;; (let ((result
;;        (resolve-vow-and-return-result
;;         b-vat
;;         (lambda ()
;;           (define-values (vow resolver)
;;             (spawn-promise-and-resolver))
;;           (define alice-vow
;;             (<- b-mycapn 'enliven alice-locator-sref))
;;           (on alice-vow
;;               (lambda (alice)
;;                 ;; Do the on-sever
;;                 (on-sever
;;                  alice
;;                  (lambda (type reason)
;;                    (<-np resolver 'fulfill (list type reason))))
;;                 ;; Then halt the netlayer
;;                 (<-np a-netlayer 'halt)))
;;           vow))))
;;   (test-equal "Can halt the testuds netlayer and get on-sever callback"
;;     #(ok (disconnect "Remote disconnected"))
;;     result))

(test-end "test-testuds-netlayer")
