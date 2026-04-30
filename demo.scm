(use-modules (dom)
             (editor)
             (fibers scheduler)
             (hoot ffi)
             (ice-9 match))

(define *tree* (sexp->tree '(+ 1 (* 2 3 (- 4 5)) 6)))

(define (exp-id id)
  (string-append "exp-" (number->string id)))

(define* (tree->shtml tree #:optional (root (tree-root tree)))
  (define (focus-maybe id)
    (when id
      (focus! (get-element-by-id (exp-id id)))))
  ;; TODO: Use browser range/selection API to move cursor to the end.
  (define (edit id orig)
    (match (tree-ref *tree* id)
      (($ <node:atom> parent value)
       (define (commit)
         (match (element-inner-text new)
           (""
            (let* ((parent-id (tree-parent *tree* id))
                   (focus-id (tree-prev *tree* id))
                   (tree (tree-delete tree id))
                   (new-elem (shtml->dom (tree->shtml tree parent-id))))
              (set! *tree* tree)
              (replace-with! (get-element-by-id (exp-id parent-id)) new-elem)
              (focus! (get-element-by-id (exp-id focus-id)))))
           (value
            (let ((new-elem (shtml->dom (node-element id "atom" (list value))))
                  (tree (tree-set *tree* id (make-atom-node parent value))))
              (set! *tree* tree)
              (replace-with! new new-elem)
              (focus! new-elem)))))
       (define new
         (shtml->dom
          `(span (@ (class "editor-node editor-atom")
                    (contenteditable "plaintext-only")
                    ;; (type "text")
                    ;; (value ,(match value
                    ;;           ((? number? x) (number->string x))
                    ;;           ((? symbol? sym) (symbol->string sym))
                    ;;           (str str)))
                    (focusout ,(lambda (e) (commit)))
                    (keydown ,(lambda (e)
                                (stop-propagation! e)
                                (match (keyboard-event-key e)
                                  ("Enter"
                                   (commit)
                                   (prevent-default! e))
                                  ("Escape"
                                   (replace-with! new orig)
                                   (focus! orig)
                                   (prevent-default! e))
                                  (_ (values))))))
                 ,value)))
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
    (call-with-values (lambda () (tree-insert-atom *tree* id))
      (lambda (tree new-id)
        (let* ((parent-id (tree-parent tree new-id))
               (new-elem (shtml->dom (tree->shtml tree parent-id))))
          (set! *tree* tree)
          (replace-with! (get-element-by-id (exp-id parent-id)) new-elem)
          (edit new-id (get-element-by-id (exp-id new-id)))))))
  (define (open id elem)
    (call-with-values (lambda () (tree-open *tree* id))
      (lambda (tree new-id)
        (let* ((parent-id (tree-parent tree new-id))
               (new-elem (shtml->dom (tree->shtml tree parent-id))))
          (set! *tree* tree)
          (replace-with! (get-element-by-id (exp-id parent-id)) new-elem)
          (focus! (get-element-by-id (exp-id new-id)))))))
  (define (forward-delete id elem)
    (let ((parent-id (tree-parent tree id)))
      (call-with-values (lambda () (tree-forward-delete *tree* id))
        (lambda (tree focus-id)
          (unless (eq? tree *tree*)
            (let ((new-elem (shtml->dom (tree->shtml tree parent-id))))
              (set! *tree* tree)
              (replace-with! (get-element-by-id (exp-id parent-id)) new-elem)))
          (focus! (get-element-by-id (exp-id focus-id)))))))
  (define (backward-delete id elem)
    (let ((parent-id (tree-parent tree id)))
      (call-with-values (lambda () (tree-backward-delete *tree* id))
        (lambda (tree removed-id)
          (unless (eq? tree *tree*)
            (let ((new-elem (shtml->dom (tree->shtml tree parent-id))))
              (set! *tree* tree)
              (replace-with! (get-element-by-id (exp-id parent-id)) new-elem)
              (focus! (get-element-by-id (exp-id id)))))))))
  (define (delimiter str)
    `(span (@ (class "editor-delimiter")) ,str))
  (define (node-element id type contents)
    `(span (@ (id ,(exp-id id))
              (class ,(string-append "editor-node editor-" type))
              ;; (contenteditable ,(if (equal? type "atom") "true" "false"))
              (tabindex "0")
              (click ,(lambda (e)
                        (focus! (event-target e))))
              ;; (input ,(lambda (e) (pk 'input e)))
              (keydown ,(lambda (e)
                          (stop-propagation! e)
                          (match (pk 'key (keyboard-event-key e))
                            ("ArrowLeft"
                             (focus-maybe
                              (if (keyboard-event-ctrl-key? e)
                                  (tree-prev-sexp *tree* id)
                                  (tree-prev *tree* id))))
                            ("ArrowRight"
                             (focus-maybe
                              (if (keyboard-event-ctrl-key? e)
                                  (tree-next-sexp *tree* id)
                                  (tree-next *tree* id))))
                            ("ArrowUp"
                             (focus-maybe (tree-parent *tree* id)))
                            ("Enter"
                             (match type
                               ("atom"
                                (edit id (event-target e))
                                (prevent-default! e))
                               ("cons"
                                (insert id (event-target e)))
                               (_ (values))))
                            ("("
                             (if (keyboard-event-alt-key? e)
                                 (wrap id (event-target e))
                                 (open id (event-target e))))
                            (" "
                             (prevent-default! e)
                             (insert id (event-target e)))
                            ("Delete"
                             (forward-delete id (event-target e)))
                            ("Backspace"
                             (backward-delete id (event-target e)))
                            ((or "Ctrl" "Alt" "Shift")
                             (values))
                            (_ (values))))))
           ,@contents))
  (let lp ((id root))
    (match (tree-ref tree id)
      (($ <node:atom> _ value)
       (node-element
        id "atom"
        (if (null? value)
            '("()")
            (list value))))
      (($ <node:compound> _ 'list ())
       (node-element id "compound" (list (delimiter "()"))))
      (($ <node:compound> _ 'list (first . rest))
       (node-element
        id "compound"
        `(,(delimiter "(")
          ,(lp first)
          ,@(apply append
                   (map (lambda (child)
                          (list " " (lp child)))
                        rest))
          ,(delimiter ")")))))))

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
    (h1 "Structural editor demo")
    (main (@ (class "editor"))
          ,(tree->shtml *tree*)))))
