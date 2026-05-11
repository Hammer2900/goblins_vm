;; Inspired by fructure

(define-module (editor)
  #:use-module (goblins utils hashmap)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:export (<node:atom>
            make-atom-node
            node-atom?
            atom-node-parent
            atom-node-ref

            <node:comment>
            make-comment-node
            comment-node?
            comment-node-parent
            comment-node-string
            comment-node-inline?

            <node:eol>
            eol-node?
            eold-node-parent

            <node:compound>
            compound-node?
            compound-node-parent
            compound-node-type
            compound-node-children
            compound-node-eol

            node-parent
            set-node-parent

            make-empty-tree
            tree?
            tree-root
            set-tree-root
            tree-nodes
            tree-ref
            tree-set
            tree-set-atom
            tree-next
            tree-prev
            tree-parent
            tree-next-sexp
            tree-prev-sexp
            tree-wrap
            tree-open
            tree-insert-atom
            tree-next-eol
            tree-delete
            tree-forward-delete
            tree-backward-delete
            tree->sexp
            sexp->tree))

;; Nodes hold expressions and refer back to their parent node id.

;; Booleans, numbers, symbols, keywords, and strings
(define-record-type <node:atom>
  (make-atom-node parent value)
  atom-node?
  (parent atom-node-parent)
  (value atom-node-ref))

(define-record-type <node:comment>
  (make-comment-node parent string inline?)
  comment-node?
  (parent comment-node-parent)
  (string comment-node-string)
  (inline? comment-node-inline?))

(define-record-type <node:eol>
  (make-eol-node parent)
  eol-node?
  (parent eol-node-parent))

(define-record-type <node:compound>
  (make-compound-node parent type children eol)
  compound-node?
  (parent compound-node-parent)
  (type compound-node-type)
  (children compound-node-children)
  (eol compound-node-eol))

(define (node-parent node)
  (match node
    ((or ($ <node:atom> parent)
         ($ <node:eol> parent)
         ($ <node:compound> parent))
     parent)))

(define (set-node-parent node parent)
  (match node
    (($ <node:atom> _ value)
     (make-atom-node parent value))
    (($ <node:eol> _)
     (make-eol-node parent))
    (($ <node:compound> _ type children eol)
     (make-compound-node parent type children eol))))

(define (replace-child parent child new)
  (match parent
    (($ <node:compound> parent type children eol)
     (make-compound-node
      parent type
      (let lp ((children children))
        (match children
          (() (error "no such child" child))
          ((child* . children)
           (if (eq? child child*)
               (cons new children)
               (cons child* (lp children))))))
      eol))))

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

(define* (add-atom-node nodes value #:key parent)
  (let* ((id (fresh-id))
         (atom (make-atom-node parent value)))
    (values (hashmap-set nodes id atom) id)))

(define* (add-list-node nodes #:key (id (fresh-id)) parent (children '()))
  (let* ((eol-id (fresh-id))
         (eol (make-eol-node id))
         (list-node (make-compound-node parent 'list children eol-id))
         (nodes (hashmap-set nodes id list-node))
         (nodes (hashmap-set nodes eol-id eol)))
    (values nodes id)))

;; Not really empty, but with an empty list as the root.
(define (make-empty-tree)
  (call-with-values (lambda () (add-list-node (hashvmap)))
    (lambda (nodes root)
      (make-tree root nodes))))

(define (tree-ref tree id)
  (hashmap-ref (tree-nodes tree) id))

(define (tree-set tree id node)
  (match tree
    (($ <tree> root nodes)
     (make-tree root (hashmap-set nodes id node)))))

(define (tree-set-atom tree id value)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       (($ <node:atom> parent)
        (let ((node (make-atom-node parent value)))
          (make-tree root (hashmap-set nodes id node))))))))

(define (tree-parent tree id)
  (node-parent (tree-ref tree id)))

;; Conversion routines
(define* (tree->sexp tree #:optional (root (tree-root tree)))
  (let ((nodes (tree-nodes tree)))
    (let lp ((id root))
      (match (hashmap-ref nodes id)
        (($ <node:atom> _ value)
         value)
        (($ <node:compound> _ 'list children)
         (map lp children))
        (($ <node:compound> _ 'dotted-list children)
         (let lp ((children children))
           (match children
             ((last) last)
             ((child . children)
              (cons (lp child) (lp children))))))
        (($ <node:compound> _ 'vector children)
         (list->vector (map lp children)))))))

;; TODO: tree->syntax

(define (sexp->tree exp)
  (define (walk nodes exp id parent)
    (match exp
      ((children ...)
       (let ((ids (map (lambda (_) (fresh-id)) children)))
         (call-with-values (lambda () (add-list-node nodes #:id id #:children ids))
           (lambda (nodes id)
             (fold (lambda (child child-id nodes)
                     (walk nodes child child-id id))
                   nodes children ids)))))
      (_ (hashmap-set nodes id (make-atom-node parent exp)))))
  (let* ((root (fresh-id))
         (nodes (walk (hashvmap) exp root #f)))
    (make-tree root nodes)))

;; Forward and backward pre-order traversal of trees.
(define (tree-next tree id)
  (let lp ((id id) (prev #f))
    (and id
         (match (tree-ref tree id)
           ((or ($ <node:atom> parent)
                ($ <node:eol> parent))
            (lp parent id))
           (($ <node:compound> parent type children eol)
            (if prev
                (if (eqv? prev eol)
                    (lp parent id)
                    (let find ((children children))
                      (match children
                        (()
                         eol)
                        ((child)
                         (if (eqv? child prev)
                             eol
                             (error "no such child" prev)))
                        ((and (_ next . _) (child . children))
                         (if (eqv? child prev)
                             next
                             (find children))))))
                (match children
                  (()
                   eol)
                  ((child . _)
                   child))))))))

(define (tree-prev tree id)
  (let lp ((id id) (prev #f))
    (and id
         (match (tree-ref tree id)
           ((or ($ <node:atom> parent)
                ($ <node:eol> parent))
            (lp parent id))
           (($ <node:compound> parent type children eol)
            (if prev
                (if (eqv? prev eol)
                    (match children
                      (() id)
                      ((_ ... last)
                       (match (tree-ref tree last)
                         ((? compound-node? node)
                          (compound-node-eol node))
                         (_ last))))
                    (let find ((children children) (sibling #f))
                      (match children
                        (()
                         (error "no such child" prev))
                        ((child . children)
                         (if (eqv? child prev)
                             (if sibling
                                 (match (tree-ref tree sibling)
                                   ((? atom-node?)
                                    sibling)
                                   ((? compound-node? node)
                                    (compound-node-eol node)))
                                 id)
                             (find children child))))))
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

(define (tree-next-sexp tree id)
  (match (tree-ref tree id)
    ((? compound-node? node)
     (compound-node-eol node))
    (node
     (match (node-parent node)
       (#f #f)
       (parent
        (match (tree-ref tree parent)
          (($ <node:compound> _ _ children eol)
           (if (eqv? id eol)
               (tree-next tree id)
               (let lp ((children children))
                 (match children
                   ((_)
                    eol)
                   ((and (_ next . _) (child . children))
                    (if (eqv? child id)
                        next
                        (lp children)))))))))))))

(define (tree-prev-sexp tree id)
  (match (tree-parent tree id)
    (#f #f)
    (parent
     (match (tree-ref tree parent)
       (($ <node:compound> _ _ children eol)
        (if (eqv? id eol)
            parent
            (let lp ((children children) (prev #f))
              (match children
                ((child . children)
                 (if (eqv? child id)
                     (or prev parent)
                     (lp children child)))))))))))

(define (tree-wrap tree id)
  (match tree
    (($ <tree> root nodes)
     ;; Can't wrap the root.
     (if (eqv? id root)
         (values tree id)
         (let* ((node (hashmap-ref nodes id))
                (parent (node-parent node)))
           (call-with-values (lambda ()
                               (add-list-node nodes
                                              #:parent parent
                                              #:children (list id)))
             (lambda (nodes wrapper)
               ;; Rewrite selected node to point at new parent wrapper.
               (let* ((node (set-node-parent node wrapper))
                      (nodes (hashmap-set nodes id node)))
                 ;; Either replace the root node with the new wrapper node
                 ;; or splice the new wrapper node into the middle of the
                 ;; tree somewhere.
                 (values (if (eqv? id root)
                             (make-tree wrapper nodes)
                             (let* ((parent-node (hashmap-ref nodes parent))
                                    (parent-node (replace-child parent-node id wrapper))
                                    (nodes (hashmap-set nodes parent parent-node)))
                               (make-tree root nodes)))
                         wrapper)))))))))

(define (tree-open tree id)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       ;; When a list head is selected, open a new list within.
       (($ <node:compound> parent type children eol)
        (call-with-values (lambda () (add-list-node nodes #:parent id))
          (lambda (nodes empty)
            ;; Rewrite node to add the new empty child list.
            (let* ((node (make-compound-node parent type
                                             (cons empty children)
                                             eol))
                   (nodes (hashmap-set nodes id node)))
              (values (make-tree root nodes) empty)))))
       ;; When a leaf node is selected, open a new list as a sibling
       ;; to the right.
       (node
        (let lp ((id id) (parent (node-parent node)))
          (match (hashmap-ref nodes parent)
            (($ <node:compound> grandparent type children eol)
             ;; If the end-of-list node is selected then insert the
             ;; element as a sibling of the parent, recursively.
             (if (eqv? id eol)
                 (lp parent grandparent)
                 (call-with-values (lambda () (add-list-node nodes #:parent parent))
                   (lambda (nodes empty)
                     ;; Rewrite parent node to add the new empty child
                     ;; list to the right of the selected node.
                     (let* ((node (make-compound-node
                                   grandparent type
                                   (let lp ((children children))
                                     (match children
                                       ((child . children)
                                        (if (eqv? child id)
                                            (cons* child empty children)
                                            (cons child (lp children))))))
                                   eol))
                            (nodes (hashmap-set nodes parent node)))
                       (values (make-tree root nodes) empty)))))))))))))

;; TODO: Similar logic as tree-open. Refactor.
(define (tree-insert-atom tree id value)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       (($ <node:compound> parent type children eol)
        (call-with-values (lambda () (add-atom-node nodes value #:parent id))
          (lambda (nodes atom)
            (let* ((node (make-compound-node parent type (cons atom children) eol))
                   (nodes (hashmap-set nodes id node)))
              (values (make-tree root nodes) atom)))))
       (node
        (let lp ((id id) (parent (node-parent node)))
          (match (hashmap-ref nodes parent)
            (($ <node:compound> grandparent type children eol)
             (if (eqv? id eol)
                 (lp parent grandparent)
                 (call-with-values (lambda ()
                                     (add-atom-node nodes value
                                                    #:parent parent))
                   (lambda (nodes atom)
                     (let* ((node (make-compound-node
                                   grandparent type
                                   (let lp ((children children))
                                     (match children
                                       ((child . children)
                                        (if (eqv? child id)
                                            (cons* child atom children)
                                            (cons child (lp children))))))
                                   eol))
                            (nodes (hashmap-set nodes parent node)))
                       (values (make-tree root nodes) atom)))))))))))))

(define (tree-next-eol tree id)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       ((? compound-node? node)
        (compound-node-eol node))
       (node
        (let lp ((id id) (node node))
          (match (node-parent node)
            (#f #f)
            (parent
             (let* ((parent-node (hashmap-ref nodes parent))
                    (eol (compound-node-eol parent-node)))
               (if (eqv? id eol)
                   (lp parent parent-node)
                   eol))))))))))

(define (tree-delete tree id)
  (define (delete-subtree node nodes)
    (match node
      (($ <node:compound> parent type children eol)
       (fold (lambda (child nodes)
               (delete-subtree child (hashmap-remove nodes child)))
             (hashmap-remove nodes eol) children))
      (_ nodes)))
  (match tree
    (($ <tree> root nodes)
     (let* ((node (hashmap-ref nodes id))
            (parent (node-parent node)))
       (match (hashmap-ref nodes parent)
         (#f (error "deleting root node unimplemented"))
         (($ <node:compound> grandparent type children eol)
          (if (eqv? id eol)
              tree
              (let* ((children (let lp ((children children))
                                 (match children
                                   (() (error "no such child" id))
                                   ((child . children)
                                    (if (eqv? child id)
                                        children
                                        (cons child (lp children)))))))
                     (parent-node (make-compound-node grandparent type children eol))
                     (nodes (hashmap-set nodes parent parent-node)))
                ;; Recursively delete the entire subtree so we aren't
                ;; leaving garbage behind.
                (make-tree root (delete-subtree node nodes))))))))))

(define (tree-forward-delete tree id)
  (match tree
    (($ <tree> root nodes)
     (match (hashmap-ref nodes id)
       ;; When deleting from the opening delimiter of a compound node,
       ;; just move ahead to either the first item or the EOL node and
       ;; leave the tree as-is.
       (($ <node:compound> parent type children eol)
        (match children
          (() (values tree eol))
          ((child . _) (values tree child))))
       (node
        (let ((parent (node-parent node)))
          (if parent
              (match (hashmap-ref nodes parent)
                (($ <node:compound> grandparent type children eol)
                 (if (eqv? id eol)
                     (match children
                       ;; List is empty, so delete it.
                       (()
                        (let ((focus (tree-next tree id)))
                          (values (tree-delete tree parent) focus)))
                       ;; List has at least one item, so do nothing.
                       (_ (values tree id)))
                     (let lp ((children children))
                       (match children
                         (() (error "no such child" id))
                         ((child . children)
                          (if (eqv? child id)
                              (values (tree-delete tree child)
                                      (match children
                                        (() eol)
                                        ((next . _) next)))
                              (lp children))))))))
              ;; TODO: Handle deletion of root node.
              (error "deletion of root node unimplemented"))))))))

;; TODO: Recursively remove deleted subtrees.
(define (tree-backward-delete tree id)
  (match tree
    (($ <tree> root nodes)
     (let ((parent (node-parent (hashmap-ref nodes id))))
       (match (hashmap-ref nodes parent)
         (($ <node:compound> grandparent type children eol)
          (if (eqv? id eol)
              (match children
                ;; List is empty, so delete it.
                (()
                 (let ((focus (tree-next tree id)))
                   (values (tree-delete tree parent) focus)))
                ;; List has at least one item, so delete the last item
                ;; if it is a leaf node or move to the closing
                ;; delimiter if it is a compound node.
                ((head ... tail)
                 (match (hashmap-ref nodes tail)
                   ((? compound-node? node)
                    (values tree (compound-node-eol node)))
                   (_
                    (values (tree-delete tree tail) eol)))))
              ;; If the selected node isn't EOL then we know there is
              ;; at least one child.
              (match children
                ((head . tail)
                 (if (eqv? head id)
                     ;; Don't delete anything.
                     (values tree id)
                     (let lp ((tail tail) (prev head))
                       (match tail
                         (() (error "no such child" id))
                         ((child . tail)
                          (if (eqv? child id)
                              (values (tree-delete tree prev) id)
                              (lp tail child)))))))))))))))
