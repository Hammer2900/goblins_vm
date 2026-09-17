(define-module (tests ocapn netlayer test-fake)
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer fake)
  #:use-module (goblins actor-lib inbox)
  #:use-module (goblins persistence-store memory)
  #:use-module (tests utils)
  #:use-module (fibers channels)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64))

(test-begin "test-fake-netlayer")

(define test-vat (spawn-vat #:name "test-vat"))
(define-values (test-inbox test-outbox test-halt)
  (call-with-vat test-vat spawn-inbox))
;; Tests for the ^fake-network
(define test-network
  (with-vat test-vat
    (spawn ^fake-network)))

(test-error
 "Check connecting to a non-existing network fails"
 #t
 (with-vat test-vat
   ($ test-network 'connect-to "does-not-exist")))

(define test-connection
  (with-vat test-vat
    ($ test-network 'register "test" test-outbox)
    ($ test-network 'connect-to "test")))
(define message
  (resolve-vow-and-return-result test-vat (lambda () ($ test-inbox))))
(with-vat test-vat
  (test-assert "Check we're getting back fibers channels"
    (match test-connection
      (('*outgoing-new-conn* (? channel?) (? channel?)) #t)
      (_ #f)))

  ;; This will wrap the return as #(ok return-val)
  (test-assert "Check we're getting back two fibers channels"
    (match message
      (#(ok ('*incoming-new-conn* (? channel?) (? channel?))) #t)
      (_ #f))))

;; Tests for the ^fake-netlayer
(define a-vat (spawn-vat #:name "a-vat"))
(define b-vat (spawn-vat #:name "b-vat"))
(define a-location (string->ocapn-id "ocapn://a.fake"))
(define b-location (string->ocapn-id "ocapn://b.fake"))

(define a-netlayer
  (with-vat a-vat
    (spawn-fake-netlayer test-network "a")))
(define b-netlayer
  (with-vat b-vat
    (spawn-fake-netlayer test-network "b")))

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
    ($ a-mycapn 'register alice 'fake)))
(define bob-locator-sref
  (with-vat b-vat
    ($ b-mycapn 'register bob 'fake)))

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

;; Test on-sever works in the fake netlayer
(let ((result
       (resolve-vow-and-return-result
        b-vat
        (lambda ()
          (define-values (vow resolver)
            (spawn-promise-and-resolver))
          (define alice-vow
            (<- b-mycapn 'enliven alice-locator-sref))
          (on alice-vow
              (lambda (alice)
                ;; Do the on-sever
                (on-sever
                 alice
                 (lambda (type reason)
                   (<-np resolver 'fulfill (list type reason))))
                ;; Then halt the netlayer
                (<-np a-netlayer 'halt)))
          vow))))
  (test-equal "Can halt the fake netlayer and get on-sever callback"
    #(ok (disconnect "Remote disconnected"))
    result))

;; Test with persistence
(define store (make-memory-store))
(define-values (vat network netlayer-one netlayer-two)
  (spawn-persistent-vat
   fake-netlayer-env
   (lambda ()
     (define network (spawn ^fake-network))
     (values network
             (spawn-fake-netlayer network "one")
             (spawn-fake-netlayer network "two")))
   store))

(define-values (mycapn-one mycapn-two)
  (with-vat vat
    (values (spawn-mycapn netlayer-one)
            (spawn-mycapn netlayer-two))))

(define (^greeter bcom our-name)
  (lambda (their-name)
    (format #f "Hello ~a, my name is ~a" their-name our-name)))

;; First check we can connect as per usual
(define alice-sref
  (with-vat vat
    ($ mycapn-one 'register (spawn ^greeter "Alice") 'fake)))

(test-equal "Check we can use the fake netlayer before restore"
  #(ok "Hello Bob, my name is Alice")
  (resolve-vow-and-return-result
   vat
   (lambda ()
     (<- (<- mycapn-two 'enliven alice-sref) "Bob"))))

;; Now do the restore
(vat-halt! vat)
(define-values (vat* network* netlayer-one* netlayer-two*)
  (spawn-persistent-vat
   fake-netlayer-env
   (lambda ()
     (error "Should be restoring from the store"))
   store))

;; We only persisted the netlayer, not all of CapTP so set it
;; back up. The connections between netlayer-one and netlayer-two
;; should still be functioning (this is what we're testing)
(define-values (mycapn-one* mycapn-two*)
  (with-vat vat*
    (values (spawn-mycapn netlayer-one*)
            (spawn-mycapn netlayer-two*))))

(define alice-sref*
  (with-vat vat*
    ($ mycapn-one* 'register (spawn ^greeter "Alice") 'fake)))

(test-equal "Check we can use the fake netlayer after restore"
  #(ok "Hello Bob, my name is Alice")
  (resolve-vow-and-return-result
   vat*
   (lambda ()
     (<- (<- mycapn-two* 'enliven alice-sref*) "Bob"))))

(test-end "test-fake-netlayer")
