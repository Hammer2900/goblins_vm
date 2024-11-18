(define-module (tests ocapn netlayer test-insecure-websocket)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer insecure-websocket)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils))

(test-begin "test-insecure-websocket-netlayer")

(define client-vat (spawn-vat #:name "client-vat"))
(define server-vat (spawn-vat #:name "server-vat"))

(define client-netlayer
  (with-vat client-vat
    (spawn ^insecure-websocket-client-netlayer)))
(define server-netlayer
  (with-vat server-vat
    (spawn ^insecure-websocket-server-netlayer)))

(define client-mycapn
  (with-vat client-vat (spawn-mycapn client-netlayer)))
(define server-mycapn
  (with-vat server-vat (spawn-mycapn server-netlayer)))

(define (^greeter _bcom my-name)
  (lambda (your-name)
    (format #f "Hello ~a, my name is ~a!" your-name my-name)))

(define bob
  (with-vat server-vat
    (spawn ^greeter "Bob")))

(define bob-locator-sref
  (with-vat server-vat
    ($ server-mycapn 'register bob 'insecure-websocket)))

(let ((result
       (resolve-vow-and-return-result
        client-vat
        (lambda ()
          (define bob-vow (<- client-mycapn 'enliven bob-locator-sref))
          (<- bob-vow "Alice")))))
  (test-equal "Enliven far sturdyref and use it from client->server"
    #(ok "Hello Alice, my name is Bob!")
    result))

(test-end)
