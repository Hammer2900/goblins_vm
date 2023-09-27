(use-modules (goblins)
             (goblins actor-lib cell)
             (goblins actor-lib methods)
             (ice-9 match)
             (srfi srfi-9))

(define-record-type <actor-slot>
  (make-actor-slot name default)
  actor-slot?
  (name actor-slot-name)
  (default actor-slot-default))

(define-record-type <actor-type>
  (make-actor-type slots behavior)
  actor-type?
  (slots actor-type-slots set-actor-type-slots!)
  (behavior actor-type-behavior set-actor-type-behavior!))

(define-record-type <actor-instance>
  (make-actor-instance type slots)
  actor-instance?
  (type actor-instance-type)
  (slots actor-instance-slots))

(define (actor-type-slot-ref type name)
  (or (hashq-ref (actor-type-slots type) name)
      (error "invalid slot" name)))

(define (actor-type-clear-slots! type)
  (hash-clear! (actor-type-slots type)))

(define (actor-type-add-slot! type name default)
  (hashq-set! (actor-type-slots type) name (make-actor-slot name default)))

(define (actor-slot-ref obj name)
  (let* ((slot (actor-type-slot-ref (actor-instance-type obj) name))
         (slots (actor-instance-slots obj))
         (cell (hashq-ref slots slot)))
    (if cell
        ($ cell)
        (let ((default (actor-slot-default slot)))
          (hashq-set! slots slot (spawn ^cell default))
          default))))

(define (actor-slot-set! obj name value)
  (let* ((slots (actor-instance-slots obj))
         (slot (actor-type-slot-ref (actor-instance-type obj) name))
         (cell (hashq-ref slots slot)))
    (if cell
        ($ cell value)
        (hashq-set! slots slot (spawn ^cell value)))))

(define-syntax-rule (define-actor-type <name>
                      ^constructor self
                      ((slot-name slot-default) ...)
                      (((method-name method-args ...) method-body ...) ...))
  (begin
    (define <name>
      (if (and (defined? '<name>) (actor-type? <name>))
          <name>
          (make-actor-type (make-hash-table) #f)))
    (actor-type-clear-slots! <name>)
    (actor-type-add-slot! <name> 'slot-name slot-default) ...
    (set-actor-type-behavior! <name>
                              (methods
                               ((method-name self method-args ...)
                                method-body ...)
                               ...))
    (define* (^constructor _bcom #:key (slot-name slot-default) ...)
      (let ((obj (make-actor-instance <name> (make-hash-table))))
        (actor-slot-set! obj 'slot-name slot-name) ...
        (match-lambda*
          (((? symbol? method) . args)
           (apply (actor-type-behavior <name>) method obj args)))))))

(define-actor-type <robot>
  ^robot self
  ((name "AnonBot")
   (hp 100)
   (atk 10))
  (((name)
    (actor-slot-ref self 'name))
   ((hp)
    (actor-slot-ref self 'hp))
   ((atk)
    (actor-slot-ref self 'atk))
   ((damage x)
    (actor-slot-set! self 'hp (max (- (actor-slot-ref self 'hp) x) 0)))
   ((beep) "beep")
   ((boop) "boop")))

(define a-vat (spawn-vat #:name "A"))

(define alice (with-vat a-vat (spawn ^robot #:name "AliceBot")))
