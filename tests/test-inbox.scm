(use-modules (fibers)
             (fibers channels)
             (ice-9 match)
             (ice-9 q)
             (srfi srfi-64)
             (goblins inbox)
             (tests utils))

(wrap-async-tests
 (test-begin "test-inbox")

 ;; Testing that the delivery agent queues and de-queues messages
 (run-fibers
  (lambda ()
    (define-values (enq-ch deq-ch stop?)
      (spawn-delivery-agent))
    (put-message enq-ch 'foo)
    (put-message enq-ch 'bar)
    (put-message enq-ch 'baz)
    (test-equal 'foo (get-message deq-ch))
    (test-equal 'bar (get-message deq-ch))
    (test-equal 'baz (get-message deq-ch)))
  #:drain? #t)

 (test-end "test-inbox"))
