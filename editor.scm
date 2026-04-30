;; Inspired by fructure

(define-module (editor)
  #:use-module (goblins utils hashmap)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (<node:atom>
            make-atom-node
            node-atom?
            atom-node-parent
            atom-node-ref

            <node:cons>
            node-cons?
            make-cons-node
            cons-node-car
            cons-node-cdr

            node-parent
            set-node-parent

            make-empty-tree
            tree?
            tree-root
            set-tree-root
            tree-nodes
            tree-ref
            tree-set
            tree-next
            tree-prev
            tree-wrap
            tree-insert-before
            tree->sexp
            sexp->tree))

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

;; Holes are empty spots in the expression tree.

(define-record-type <hole>
  (make-hole)
  hole?)

(define (print-hole hole port)
  (display "Ø" port))
(set-record-type-printer! <hole> print-hole)

;; Nodes hold expressions and refer back to their parent node id.

(define-record-type <node:atom>
  (make-atom-node parent value)
  atom-node?
  (parent atom-node-parent)
  (value atom-node-ref))

(define-record-type <node:cons>
  (make-cons-node parent car cdr)
  cons-node?
  (parent cons-node-parent)
  (car cons-node-car)
  (cdr cons-node-cdr))

(define (null-atom-node? node)
  (match node
    (($ <node:atom> _ ()) #t)
    (_ #f)))

(define (node-parent node)
  (match node
    (($ <node:atom> parent)
     parent)
    (($ <node:cons> parent)
     parent)))

(define (set-node-parent node parent)
  (match node
    (($ <node:atom> _ value)
     (make-atom-node parent value))
    (($ <node:cons> _ car cdr)
     (make-cons-node parent car cdr))))

(define (set-node-child node old new)
  (match node
    (($ <node:cons> parent car cdr)
     (cond
      ((eq? old car)
       (make-cons-node parent new cdr))
      ((eq? old cdr)
       (make-cons-node parent car new))
      (else
       (error "not a child" old))))))

;; Unique id generator

(define fresh-id
  (let ((counter 1))
    (lambda ()
      (let ((id counter))
        (set! counter (1+ id))
        id))))

;; Expression trees as persistent sets.  Technically it's a graph but
;; we are careful to prevent cycles in the editor.

(define-record-type <tree>
  (make-tree root nodes)
  tree?
  (root tree-root)
  (nodes tree-nodes))

(define* (make-empty-tree #:optional root)
  (make-tree root (hashvmap)))

(define (set-tree-root tree root)
  (match tree
    (($ <tree> _ nodes)
     (make-tree root nodes))))

(define (tree-ref tree id)
  (hashmap-ref (tree-nodes tree) id))

(define (tree-set tree id node)
  (match tree
    (($ <tree> root nodes)
     (make-tree root (hashmap-set nodes id node)))))

;; Conversion routines
(define (tree->sexp tree)
  (match tree
    (($ <tree> root nodes)
     (let lp ((id 0))
       (match (tree-ref nodes id)
         (($ <node:atom> _ value)
          value)
         (($ <node:cons> _ car cdr)
          (cons (lp car) (lp cdr))))))))

;; TODO: tree->syntax

(define (sexp->tree exp)
  (let ((root (fresh-id)))
    (let lp ((tree (make-empty-tree root)) (exp exp) (id root) (parent #f))
      (match exp
        ((car . cdr)
         (let* ((car-id (fresh-id))
                (cdr-id (fresh-id))
                (node (make-cons-node parent car-id cdr-id)))
           (lp (lp (tree-set tree id node) car car-id id)
               cdr cdr-id id)))
        (_ (tree-set tree id (make-atom-node parent exp)))))))

;; Forward and backward pre-order traversal of trees.
(define (tree-next tree id)
  (let lp ((id id) (prev #f))
    (and id
         (match (tree-ref tree id)
           (($ <node:atom> parent)
            (lp parent id))
           (($ <node:cons> parent car cdr)
            (if prev
                (if (eqv? prev car)
                    cdr
                    (lp parent id))
                car))))))

(define (tree-prev tree id)
  (let lp ((id id) (prev #f))
    (and id
         (match (tree-ref tree id)
           (($ <node:atom> parent)
            (lp parent id))
           (($ <node:cons> parent car cdr)
            (if prev
                (if (eqv? prev car)
                    id
                    ;; Descend the subtree to find the rightmost node.
                    (let descend ((id car))
                      (match (tree-ref tree id)
                        (($ <node:atom>)
                         id)
                        (($ <node:cons> _ _ cdr)
                         (descend cdr)))))
                (lp parent id)))))))

;; Wrap node in a one element list.
(define (tree-wrap tree id)
  (match tree
    (($ <tree> root nodes)
     (let* ((node (hashmap-ref nodes id))
            (parent-id (node-parent node))
            (car-id id)
            (cdr-id (fresh-id))
            (cons-id (fresh-id))
            (car-node (set-node-parent node cons-id))
            (cdr-node (make-atom-node cons-id '()))
            (cons-node (make-cons-node parent-id car-id cdr-id))
            (nodes (hashmap-set nodes id car-node))
            (nodes (hashmap-set nodes cdr-id cdr-node))
            (nodes (hashmap-set nodes cons-id cons-node)))
       ;; Either replace the root node with the new cons node or splice
       ;; the new cons node into the middle of the tree somewhere.
       (values (if (eqv? id root)
                   (make-tree cons-id nodes)
                   (let* ((parent (hashmap-ref nodes parent-id))
                          (parent (set-node-child parent id cons-id)))
                     (make-tree root (hashmap-set nodes parent-id parent))))
               cons-id)))))

(define (tree-insert-before tree id)
  (match tree
    (($ <tree> root nodes)
     (let* ((loc (pk 'loc (hashmap-ref nodes id))))
       (match (node-parent loc)
         (#f (error "unimplemented"))
         (parent-id
          (match (hashmap-ref nodes parent-id)
            (($ <node:cons> grandparent-id car cdr)
             (if (eq? id car)
                 (let* ((cons-id (fresh-id))
                        (car-id (fresh-id))
                        (cdr-id parent-id)
                        (car-node (make-atom-node cons-id "_"))
                        (cons-node (make-cons-node grandparent-id car-id cdr-id))
                        (nodes (hashmap-set nodes car-id car-node))
                        (nodes (hashmap-set nodes cons-id cons-node)))
                   (values (if (eqv? parent-id root)
                               (make-tree cons-id nodes)
                               (let* ((grandparent (hashmap-ref nodes grandparent-id))
                                      (grandparent (set-node-child grandparent parent-id cons-id)))
                                 (make-tree root (hashmap-set nodes grandparent-id grandparent))))
                           car-id))
                 (error "unimplemented"))))))))))

(define (tree-delete-backward tree id)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       (($ <node:atom> parent-id value)
        (match (hashmap-ref nodes parent-id)
          (#f (values tree id))
          (($ <node:cons> grandparent-id car cdr)
           ;; A backward delete from an empty list in car position deletes
           ;; itself and its parent rather than the previous node.
           (if (eqv? id car)
               (if (null? value)
                   (let* ((grandparent-node (hashmap-ref nodes grandparent-id))
                          (grandparent-node (set-node-child grandparent-node parent-id cdr))
                          (cdr-node (hashmap-ref nodes cdr))
                          (cdr-node (set-node-parent cdr-node grandparent-id))
                          (nodes (hashmap-set nodes cdr cdr-node))
                          (nodes (hashmap-remove nodes parent-id))
                          (nodes (hashmap-remove nodes id)))
                     (values (make-tree root nodes) grandparent-id))
                   (tree-delete-backward tree (tree-prev tree id)))
               ;; in cdr position, the car and parent nodes are deleted.
               (let* ((grandparent-node (hashmap-ref nodes grandparent-id))
                      (grandparent-node (set-node-child grandparent-node parent-id cdr))
                      (cdr-node (hashmap-ref nodes cdr))
                      (cdr-node (set-node-parent cdr-node grandparent-id))
                      (nodes (hashmap-set nodes cdr cdr-node))
                      (nodes (hashmap-remove nodes parent-id))
                      (nodes (hashmap-remove nodes car)))
                 (values (make-tree root nodes) grandparent-id)))))))))


  (match (tree-prev tree id)
    (#f (values tree #f))
    (prev
     (match (tree-ref tree prev)
       ((? cons-node?)
        (values tree prev))
       ((? atom-node?)
        (values ))))))


;; (make-compound-node 'list '(1 2 3))


;; (<()> . ()) -> ()
;; (() . <()>) -> ()

;; <(a . (b . ()))> -> (a . (b . ()))
;; (<a> . (b . ())) -> (a . (b . ()))

;; (a . <(b . ())>) -> (b . ())
;; (a . (<b> . ())) -> (b . ())

;; (a . (b . <()>)) -> (a . ())


;; (+ 1 (* 2 3 (- 4 5)) 6)
