;; Inspired by fructure

(define-module (editor)
  #:use-module (goblins utils hashmap)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (<node:atom>
            make-atom-node
            node-atom?
            atom-node-parent
            atom-node-ref

            ;; <node:cons>
            ;; node-cons?
            ;; make-cons-node
            ;; cons-node-car
            ;; cons-node-cdr

            <node:compound>
            compound-node?
            compound-node-parent
            compound-node-type
            compound-node-children

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
            tree-parent
            tree-next-sexp
            tree-prev-sexp
            tree-wrap
            tree-open
            tree-insert-atom
            tree-delete
            tree-forward-delete
            tree-backward-delete
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

(define-record-type <node:compound>
  (make-compound-node parent type children)
  compound-node?
  (parent compound-node-parent)
  (type compound-node-type)
  (children compound-node-children))

;; (define-record-type <node:cons>
;;   (make-cons-node parent car cdr)
;;   cons-node?
;;   (parent cons-node-parent)
;;   (car cons-node-car)
;;   (cdr cons-node-cdr))

;; (define (null-atom-node? node)
;;   (match node
;;     (($ <node:atom> _ ()) #t)
;;     (_ #f)))

(define (node-parent node)
  (match node
    (($ <node:atom> parent)
     parent)
    (($ <node:compound> parent)
     parent)))

(define (set-node-parent node parent)
  (match node
    (($ <node:atom> _ value)
     (make-atom-node parent value))
    (($ <node:compound> _ type children)
     (make-compound-node parent type children))))

(define (replace-child parent child new)
  (match parent
    (($ <node:compound> parent type children)
     (make-compound-node
      parent type
      (let lp ((children children))
        (match children
          (() (error "no such child" child))
          ((child* . children)
           (if (eq? child child*)
               (cons new children)
               (cons child* (lp children))))))))))

;; (define (set-node-child node old new)
;;   (match node
;;     (($ <node:cons> parent car cdr)
;;      (cond
;;       ((eq? old car)
;;        (make-cons-node parent new cdr))
;;       ((eq? old cdr)
;;        (make-cons-node parent car new))
;;       (else
;;        (error "not a child" old))))))

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

;; (define* (make-empty-tree #:optional root)
;;   (make-tree root (hashvmap)))

;; (define (set-tree-root tree root)
;;   (match tree
;;     (($ <tree> _ nodes)
;;      (make-tree root nodes))))

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
     (let lp ((id root))
       (match (hashmap-ref nodes id)
         (($ <node:atom> _ value)
          value)
         (($ <node:compound> _ 'list children)
          (map lp children)))))))

;; TODO: tree->syntax

(define (sexp->tree exp)
  (define (walk nodes exp id parent)
    (match exp
      ((children ...)
       (let* ((ids (map (lambda (_) (fresh-id)) children))
              (node (make-compound-node parent 'list ids)))
         (fold (lambda (child child-id nodes)
                 (walk nodes child child-id id))
               (hashmap-set nodes id node) children ids)))
      (_ (hashmap-set nodes id (make-atom-node parent exp)))))
  (let ((root (fresh-id)))
    (make-tree root (walk (hashvmap) exp root #f))))

;; Forward and backward pre-order traversal of trees.
(define (tree-next tree id)
  (let lp ((id id) (prev #f))
    (and id
         (match (tree-ref tree id)
           ((or ($ <node:atom> parent)
                ($ <node:compound> parent _ ()))
            (lp parent id))
           (($ <node:compound> parent type children)
            (if prev
                (let find ((children children))
                  (match children
                    (()
                     (error "no such child" prev))
                    ((child)
                     (if (eqv? child prev)
                         (lp parent id)
                         (error "no such child" prev)))
                    ((and (_ next . _) (child . children))
                     (if (eqv? child prev)
                         next
                         (find children)))))
                (car children)))))))

(define (tree-prev tree id)
  (let lp ((id id) (next #f))
    (and id
         (match (tree-ref tree id)
           (($ <node:atom> parent)
            (lp parent id))
           (($ <node:compound> parent type children)
            (if next
                (let find ((children children) (prev #f))
                  (match children
                    (()
                     (error "no such child" next))
                    ((child . children)
                     (if (eqv? child next)
                         (if prev
                             ;; Descend the subtree to find the rightmost node.
                             (let descend ((id prev))
                               (match (tree-ref tree id)
                                 ((or ($ <node:atom>)
                                      ($ <node:compound> _ _ ()))
                                  id)
                                 (($ <node:compound> _ _ (_ ... last))
                                  (descend last))))
                             id)
                         (find children child)))))
                (lp parent id)))))))

;; Test

;; (let* ((tree (sexp->tree '(+ 1 (* 2 3 (- 4 5)) 6)))
;;        (forward (let lp ((id (tree-root tree)))
;;                   (let ((next (tree-next tree id)))
;;                     (if next
;;                         (cons id (lp next))
;;                         (list id)))))
;;        (last (match forward ((_ ... last) last)))
;;        (backward (let lp ((id last))
;;                    (let ((prev (tree-prev tree id)))
;;                      (if prev
;;                          (cons id (lp prev))
;;                          (list id))))))
;;   (equal? forward (reverse backward)))

(define (tree-parent tree id)
  (node-parent (tree-ref tree id)))

(define (tree-next-sexp tree id)
  (match (tree-parent tree id)
    (#f #f)
    (parent-id
     (match (tree-ref tree parent-id)
       (($ <node:compound> _ _ children)
        (let lp ((children children))
          (match children
            ((_)
             (tree-next-sexp tree parent-id))
            ((and (_ next . _) (child . children))
             (if (eqv? child id)
                 next
                 (lp children))))))))))

(define (tree-prev-sexp tree id)
  (match (tree-parent tree id)
    (#f #f)
    (parent-id
     (match (tree-ref tree parent-id)
       (($ <node:compound> _ _ children)
        (let lp ((children children) (prev #f))
          (match children
            ((child . children)
             (if (eqv? child id)
                 (or prev parent-id)
                 (lp children child))))))))))

(define (tree-wrap tree id)
  (match tree
    (($ <tree> root nodes)
     (let* ((node (hashmap-ref nodes id))
            (parent-id (node-parent node))
            (wrapper-id (fresh-id))
            (wrapper (make-compound-node parent-id 'list (list id)))
            (nodes (hashmap-set nodes wrapper-id wrapper))
            (nodes (hashmap-set nodes id (set-node-parent node wrapper-id))))
       ;; Either replace the root node with the new wrapper node or
       ;; splice the new wrapper node into the middle of the tree
       ;; somewhere.
       (values (if (eqv? id root)
                   (make-tree wrapper-id nodes)
                   (let* ((parent (hashmap-ref nodes parent-id))
                          (parent (replace-child parent id wrapper-id)))
                     (make-tree root (hashmap-set nodes parent-id parent))))
               wrapper-id)))))

(define (tree-open tree id)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       (($ <node:compound> parent-id type children)
        (let* ((new-id (fresh-id))
               (new (make-compound-node id 'list '()))
               (parent (make-compound-node parent-id type (cons new-id children)))
               (nodes (hashmap-set nodes new-id new))
               (nodes (hashmap-set nodes id parent)))
          (values (make-tree root nodes) new-id)))
       (node
        (let ((parent-id (node-parent node)))
          (match (hashmap-ref nodes parent-id)
            (($ <node:compound> _ _ children)
             (let* ((new-id (fresh-id))
                    (new (make-compound-node parent-id 'list '()))
                    (parent (make-compound-node
                             id 'list
                             (let lp ((children children))
                               (match children
                                 ((child . children)
                                  (if (eqv? child id)
                                      (cons* child new-id children)
                                      (cons child (lp children))))))))
                    (nodes (hashmap-set nodes new-id new))
                    (nodes (hashmap-set nodes parent-id parent)))
               (values (make-tree root nodes) new-id))))))))))

;; TODO: This is almost exactly the same as tree-open. Refactor.
(define (tree-insert-atom tree id)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       (($ <node:compound> parent-id type children)
        (let* ((new-id (fresh-id))
               (new (make-atom-node id ""))
               (parent (make-compound-node parent-id type (cons new-id children)))
               (nodes (hashmap-set nodes new-id new))
               (nodes (hashmap-set nodes id parent)))
          (values (make-tree root nodes) new-id)))
       (node
        (let ((parent-id (node-parent node)))
          (match (hashmap-ref nodes parent-id)
            (($ <node:compound> _ _ children)
             (let* ((new-id (fresh-id))
                    (new (make-atom-node parent-id ""))
                    (parent (make-compound-node
                             id 'list
                             (let lp ((children children))
                               (match children
                                 ((child . children)
                                  (if (eqv? child id)
                                      (cons* child new-id children)
                                      (cons child (lp children))))))))
                    (nodes (hashmap-set nodes new-id new))
                    (nodes (hashmap-set nodes parent-id parent)))
               (values (make-tree root nodes) new-id))))))))))

(define (tree-delete tree id)
  (match tree
    (($ <tree> root nodes)
     (let* ((node (hashmap-ref nodes id))
            (parent-id (node-parent node)))
       (match (hashmap-ref nodes parent-id)
         (#f (error "deleting root node unimplemented"))
         (($ <node:compound> grandparent-id type children)
          (let* ((children (let lp ((children children))
                             (match children
                               (() (error "no such child" id))
                               ((child . children)
                                (if (eqv? child id)
                                    children
                                    (cons child (lp children)))))))
                 (parent (make-compound-node grandparent-id type children))
                 (node (set-node-parent node #f))
                 (nodes (hashmap-set nodes parent-id parent))
                 (nodes (hashmap-set nodes id node)))
            (make-tree root nodes))))))))

(define (tree-forward-delete tree id)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       ((and node (or ($ <node:atom> parent-id)
                      ($ <node:compound> parent-id _ ())))
        ;; TODO: Handle deletion of root node.
        (match (hashmap-ref nodes parent-id)
          (($ <node:compound> grandparent-id type children)
           (call-with-values
               (lambda ()
                 (let lp ((children children) (prev #f))
                   (match children
                     ((child)
                      (if (eqv? child id)
                          (values '() (or prev parent-id))
                          ;; This shouldn't happen.
                          (error "no such child" id)))
                     ((child . children)
                      (if (eqv? child id)
                          (match children
                            ((next . _)
                             (values children next)))
                          (call-with-values (lambda () (lp children child))
                            (lambda (children focus-id)
                              (values (cons child children) focus-id))))))))
             (lambda (children focus-id)
               (let* ((parent (make-compound-node grandparent-id type children))
                      ;; Unsure if we should do this as it would make
                      ;; deletion not constant time.  Maybe we should
                      ;; periodically GC trees to shake out all the
                      ;; unreachable nodes.  By not deleting we also
                      ;; get constant time cut/paste since all the
                      ;; nodes are still in the graph.
                      ;;
                      ;; ;; Recursively remove entire subtree.
                      ;; (nodes (let lp ((id id) (nodes nodes))
                      ;;          (match (hashmap-ref nodes id)
                      ;;            ((? atom-node?)
                      ;;             (hashmap-remove nodes id))
                      ;;            (($ <node:compound> _ _ children)
                      ;;             (fold lp (hashmap-remove nodes id) children)))))
                      (nodes (hashmap-set nodes id (set-node-parent node #f)))
                      (nodes (hashmap-set nodes parent-id parent)))
                 (values (make-tree root nodes) focus-id)))))))
       (($ <node:compound> _ _ (first-child . _))
        (values tree first-child))))))

(define (tree-backward-delete tree id)
  (match tree
    (($ <tree> root nodes)
     (let ((parent-id (node-parent (hashmap-ref nodes id))))
       (match (hashmap-ref nodes parent-id)
         (($ <node:compound> grandparent-id type children)
          (call-with-values
              (lambda ()
                (match children
                  (((? (lambda (child) (eqv? child id)) child) . _)
                   (values children #f))
                  ((child . children)
                   (let lp ((children children) (prev child))
                     (match children
                       (() (error "no such child" id))
                       ((child . rest)
                        (if (eqv? child id)
                            (values children prev)
                            (call-with-values (lambda () (lp rest child))
                              (lambda (children target-id)
                                (values (cons prev children) target-id))))))))))
            (lambda (children* target-id)
              (if (eq? children* children)
                  (values tree #f)      ; no change
                  (let* ((target (hashmap-ref nodes target-id))
                         (parent (make-compound-node grandparent-id type children*))
                         (nodes (hashmap-set nodes target-id (set-node-parent target #f)))
                         (nodes (hashmap-set nodes parent-id parent)))
                    (values (make-tree root nodes) target-id)))))))))))
