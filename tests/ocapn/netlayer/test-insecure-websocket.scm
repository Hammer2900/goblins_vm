(define-module (tests ocapn netlayer test-insecure-websocket)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer insecure-websocket)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils))

(test-begin "test-insecure-websocket-netlayer")

(define a-vat (spawn-vat #:name "a-vat"))
(define b-vat (spawn-vat #:name "b-vat"))

(define a-netlayer
  (with-vat a-vat
    (spawn ^insecure-websocket-netlayer
           #:port 9999)))
(define b-netlayer
  (with-vat b-vat
    (spawn ^insecure-websocket-netlayer
           #:port 8888)))

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
    ($ a-mycapn 'register alice 'websocket)))
(define bob-locator-sref
  (with-vat b-vat
    ($ b-mycapn 'register bob 'websocket)))

(let ((result
       (resolve-vow-and-return-result
        a-vat
        (lambda ()
          (define bob-vow (<- a-mycapn 'enliven bob-locator-sref))
          (<- bob-vow "Arthur")))))
  (test-equal "Enliven far sturdyref and use it from a->b"
    #(ok "Hello Arthur, my name is Bob!")
    result))

(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda ()
          (define alice-vow (<- b-mycapn 'enliven alice-locator-sref))
          (<- alice-vow "Bun")))))
  (test-equal "Enliven far sturdyref and use it from b->a"
    #(ok "Hello Bun, my name is Alice!")
    result))

(test-end)

