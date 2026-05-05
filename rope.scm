(define-module (rope)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))


;; Simple rope implementation for string editing.

(define-record-type <rope>
  (make-rope weight left right)
  %rope?
  (weight rope-weight)
  (left rope-left)
  (right rope-right))

(define (print-rope rope port)
  (format port "#<rope ~s>" (rope->string rope)))
(set-record-type-printer! <rope> print-rope)

(define (rope? x)
  (or (%rope? x) (string? x)))

(define (rope->sexp rope)
  (let lp ((rope rope))
    (match rope
      (($ <rope> weight left right)
       (list weight (lp left) (lp right)))
      (_ rope))))

(define (rope-for-each proc rope)
  (match rope
    (($ <rope> weight left right)
     (rope-for-each proc left)
     (rope-for-each proc right))
    ("" (values))
    (str (proc str))))

(define (rope-fold proc init rope)
  (let lp ((rope rope) (prev init))
    (match rope
      (($ <rope> weight left right)
       (lp right (lp left prev)))
      ("" prev)
      (str (proc str prev)))))

(define (rope-fold-right proc init rope)
  (let lp ((rope rope) (prev init))
    (match rope
      (($ <rope> weight left right)
       (lp left (lp right prev)))
      ("" prev)
      (str (proc str prev)))))

(define (rope->list rope)
  (rope-fold-right cons '() rope))

(define (rope->string rope)
  (string-concatenate (rope->list rope)))

(define (rope-length rope)
  (match rope
    (($ <rope> weight left right)
     (+ weight (rope-length right)))
    (str (string-length str))))

(define (rope-ref rope i)
  (match rope
    (($ <rope> weight left right)
     (if (< i weight)
         (rope-ref left i)
         (rope-ref right (- i weight))))
    (str (string-ref str i))))

(define (rope-rebalance-maybe rope)
  (define (balanced? rope)
    (match rope
      (($ <rope> weight left right)
       (match (balanced? left)
         (#f #f)
         (hl
          (match (balanced? right)
            (#f #f)
            (hr
             (and (<= (abs (- hl hr)) 1)
                  (1+ (max hl hr))))))))
      ("" 0)
      (_ 1)))
  (define (rebalance rope)
    (let ((strs (list->vector (rope->list rope))))
      (let lp ((start 0) (end (vector-length strs)))
        (let ((delta (- end start)))
          (cond
           ((= delta 1)
            (vector-ref strs start))
           ((= delta 2)
            (let ((a (vector-ref strs start))
                  (b (vector-ref strs (1+ start))))
              (make-rope (rope-length a) a b)))
           (else
            (let* ((mid (+ start (quotient delta 2)))
                   (a (lp start mid))
                   (b (lp mid end)))
              (make-rope (rope-length a) a b))))))))
  (if (balanced? rope)
      rope
      (rebalance rope)))

(define (rope-append a b . rest)
  (define (concat a b)
    (make-rope (rope-length a) a b))
  (rope-rebalance-maybe
   (let lp ((a (concat a b)) (rest rest))
     (match rest
       (() a)
       ((b . rest)
        (lp (concat a b) rest))))))

(define (rope-split rope i)
  (match rope
    (($ <rope> weight left right)
     (cond
      ((< i weight)
       (call-with-values (lambda () (rope-split left i))
         (lambda (left right*)
           (values left (rope-append right* right)))))
      ((> i weight)
       (call-with-values (lambda () (rope-split right (- i weight)))
         (lambda (left* right)
           (values (rope-append left left*) right))))
      (else
       (values left right))))
    (str
     (cond
      ((zero? i)
       (values "" str))
      ((= i (string-length str))
       (values str ""))
      (else
       (values (substring str 0 i)
               (substring str i)))))))

(define (rope-insert rope i str)
  (cond
   ((zero? i)
    (rope-append str rope))
   ((= i (rope-length rope))
    (rope-append rope str))
   (else
    (call-with-values (lambda () (rope-split rope i))
      (lambda (l r)
        (rope-append l str r))))))

(define* (rope-remove rope start #:optional (end (rope-length rope)))
  (call-with-values (lambda () (rope-split rope start))
    (lambda (left right*)
      (call-with-values (lambda () (rope-split rope end))
        (lambda (left* right)
          (rope-append left right))))))

(define* (rope-select rope start #:optional (end (rope-length rope)))
  (rope-remove (rope-remove rope end) 0 start))
