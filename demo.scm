(use-modules (dom)
             (editor)
             (hoot ffi)
             (ice-9 match))

(define *tree* (sexp->tree '(+ 1 (* 2 3 (- 4 5)) 6)))

(define (exp-id id)
  (string-append "exp-" (number->string id)))

(define* (tree->shtml tree #:optional (root (tree-root tree)))
  (define (focus-maybe id)
    (when id
      (focus! (get-element-by-id (exp-id id)))))
  (define (edit id orig)
    (define node )
    (match (tree-ref *tree* id)
      (($ <node:atom> parent value)
       (define new
         (shtml->dom
          `(input (@ (type "text")
                     (value ,(match value
                               ((? number? x) (number->string x))
                               ((? symbol? sym) (symbol->string sym))
                               (str str)))
                     (keydown ,(lambda (e)
                                 (stop-propagation! e)
                                 (match (keyboard-event-key e)
                                   ("Enter"
                                    (let* ((value (element-value new))
                                           (new* (shtml->dom (node-element id "atom" (list value)))))
                                      (set! *tree* (tree-set *tree* id (make-atom-node parent value)))
                                      (replace-with! new new*)
                                      (focus! new*)
                                      (prevent-default! e)))
                                   ("Escape"
                                    (replace-with! new orig)
                                    (focus! orig)
                                    (prevent-default! e))
                                   (_ (values)))))))))
       (replace-with! orig new)
       (focus! new))))
  (define (wrap id elem)
    (call-with-values (lambda () (tree-wrap *tree* id))
      (lambda (tree new-id)
        (let ((new-elem (shtml->dom (tree->shtml tree new-id))))
          (set! *tree* tree)
          (replace-with! elem new-elem)
          (focus! new-elem)))))
  (define (insert id elem)
    (pk 'insert id)
    (call-with-values (lambda () (tree-insert-before *tree* id))
      (lambda (tree new-id)
        (let ((new-elem (shtml->dom (tree->shtml tree new-id))))
          (set! *tree* tree)
          (replace-with! elem new-elem)
          (focus! new-elem)))))
  (define (open id elem)
    (pk 'todo:open))
  (define (node-element id type contents)
    `(span (@ (id ,(exp-id id))
              (class ,(string-append "editor-node editor-" type))
              (tabindex "0")
              (click ,(lambda (e)
                        (focus! (event-target e))))
              (keydown ,(lambda (e)
                          (stop-propagation! e)
                          (match (pk 'key (keyboard-event-key e))
                            ("ArrowLeft" (focus-maybe (tree-prev *tree* id)))
                            ("ArrowRight" (focus-maybe (tree-next *tree* id)))
                            ("Enter"
                             (match type
                               ("atom" (edit id (event-target e)))
                               ("cons" (insert id (event-target e)))
                               (_ (values))))
                            ("("
                             (if (keyboard-event-alt-key? e)
                                 (wrap id (event-target e))
                                 (open id (event-target e))))
                            (_ (values))))))
           ,@contents))
  (let lp ((node (tree-ref tree root)) (id root))
    (match node
      (($ <node:atom> _ value)
       (node-element
        id "atom"
        (if (null? value)
            '("()")
            (list value))))
      ((and node ($ <node:cons>))
       (node-element
        id "cons"
        `("("
          ,@(let lp2 ((node node) (id id) (first? #t))
              (match node
                (($ <node:atom> _ ())
                 (pk 'null id)
                 (list (node-element id "atom" '(")"))))
                ((and node ($ <node:cons> _ car cdr))
                 `(,@(if first?
                         '()
                         (list (node-element id "cons" '(" "))))
                   ,(lp (tree-ref tree car) car)
                   ,@(lp2 (tree-ref tree cdr) cdr #f)))
                (node
                 `(" . " ,(lp node id) ")"))))))))))

(define body (document-body))

(add-event-listener!
 body "keydown"
 (procedure->external
  (lambda (e)
    (pk 'keydown (keyboard-event-key e)))))

(append-child!
 body
 (shtml->dom
  `(article
    (h1 "Text editor demo")
    (main (@ (class "editor"))
          ,(tree->shtml *tree*)))))
