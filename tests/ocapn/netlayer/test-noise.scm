(define-module (tests ocapn netlayer test-noise)
  #:use-module (ice-9 match)
  #:use-module (goblins)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer noise)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (goblins actor-lib cell)
  #:use-module (tests utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (srfi srfi-64))

(test-begin "test-noise-netlayer")

;; Setup the fake netlayer
(define test-vat (spawn-vat #:name "test-vat"))
(define test-channel (make-channel))
(define test-network (with-vat test-vat (spawn ^fake-network)))

(define (make-fake-netlayer vat name)
  (define new-conn-ch (make-channel))
  (with-vat test-vat ($ test-network 'register name new-conn-ch))
  (with-vat vat
    (spawn ^fake-netlayer name test-network new-conn-ch)))

(define a-vat (spawn-vat #:name "a-vat"))
(define a-fake-netlayer (make-fake-netlayer a-vat "a"))
(define a-noise-netlayer
  (with-vat a-vat (spawn ^noise-netlayer a-fake-netlayer)))
(define a-mycapn (with-vat a-vat (spawn-mycapn a-noise-netlayer)))
(define a-cell (with-vat a-vat (spawn ^cell 'apple)))
(define a-cell-sref (with-vat a-vat ($ a-mycapn 'register a-cell 'noise)))

(define b-vat (spawn-vat #:name "b-vat"))
(define b-fake-netlayer (make-fake-netlayer b-vat "b"))
(define b-noise-netlayer
  (with-vat b-vat (spawn ^noise-netlayer b-fake-netlayer)))
(define b-mycapn (with-vat b-vat (spawn-mycapn b-noise-netlayer)))
(define remote-cell
  (with-vat b-vat
    (<- b-mycapn 'enliven a-cell-sref)))
(test-assert "Check we can enliven a remote refr for the noise netlayer"
  (match (resolve-vow-and-return-result b-vat (lambda () remote-cell))
    (#(ok (? remote-object-refr?)) #t)
    (_ #f)))

(test-equal "Check we can message the remote refr using the noise netlayer"
  #(ok apple)
  (resolve-vow-and-return-result b-vat (lambda () (<- remote-cell))))

(test-assert "Check a noise sturdyref can be encoded to a string"
  (match (resolve-vow-and-return-result a-vat (lambda () a-cell-sref))
    (#(ok cell-sref) (string? (ocapn-id->string cell-sref)))
    (_ #f)))
(test-end "test-noise-netlayer")
