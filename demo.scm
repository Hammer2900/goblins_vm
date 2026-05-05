(use-modules (dom)
             (editor)
             (fibers scheduler)
             (hoot ffi)
             (ice-9 match))

(define *tree*
  ;;(sexp->tree '((* 4 5 6) + 1 (* 2 3 (- 4 5)) 6))
  (make-empty-tree))

(define (exp-id id)
  (string-append "exp-" (number->string id)))

(define (focus-maybe id)
  (when id
    (focus! (get-element-by-id (exp-id id)))))

(define (open-delimiter str)
  `(span (@ (class "editor-delimiter editor-delimiter-open")) ,str))

(define (close-delimiter id str)
  (node-element id 'close-delimiter (list str)))

(define (node-element id type contents)
  `(span (@ (id ,(exp-id id))
            (class ,(string-append
                     "editor-node "
                     (match type
                       ('atom "editor-atom")
                       ('compound "editor-compound")
                       ('close-delimiter
                        "editor-delimiter editor-delimiter-close"))))
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
                           (focus-maybe (tree-prev-sexp *tree* id)))
                          ("ArrowDown"
                           (focus-maybe (tree-next-sexp *tree* id)))
                          ("Enter"
                           (match type
                             ("atom"
                              (edit id (event-target e))
                              (prevent-default! e))
                             ;; ("cons"
                             ;;  (insert id (event-target e)))
                             (_ (values))))
                          ("("
                           (if (keyboard-event-alt-key? e)
                               (wrap id (event-target e))
                               (open id (event-target e))))
                          (")"
                           (jump-to-eol id))
                          (" "
                           (prevent-default! e)
                           (insert id "" (event-target e)))
                          ("Delete"
                           (forward-delete id (event-target e)))
                          ("Backspace"
                           (backward-delete id (event-target e)))
                          ((or "Ctrl" "Alt" "Shift")
                           (values))
                          (key
                           (when (and (= (string-length key) 1)
                                      (not (keyboard-event-ctrl-key? e))
                                      (not (keyboard-event-alt-key? e)))
                             (prevent-default! e)
                             (insert id key (event-target e))))))))
         ,@contents))

;; TODO: Use browser range/selection API to move cursor to the end.
(define (edit id orig)
  (match (tree-ref *tree* id)
    (($ <node:atom> parent value)
     (define (commit-maybe)
       (match (element-inner-text new)
         ((or "" "\n") #f)
         (value
          (let ((new-elem (shtml->dom (node-element id 'atom (list value))))
                (tree (tree-set *tree* id (make-atom-node parent value))))
            (set! *tree* tree)
            (replace-with! new new-elem)
            (focus! new-elem))
          #t)))
     (define (delete)
       (let* ((parent (tree-parent *tree* id))
              (focus (tree-prev *tree* id))
              (tree (tree-delete *tree* id))
              (new-elem (shtml->dom (tree->shtml tree parent))))
         (set! *tree* tree)
         (replace-with! (get-element-by-id (exp-id parent)) new-elem)
         (focus! (get-element-by-id (exp-id focus)))
         focus))
     (define (commit-or-delete)
       (if (commit-maybe)
           id
           (delete)))
     (define new
       (shtml->dom
        `(span (@ (class "editor-node editor-atom")
                  (contenteditable "plaintext-only")
                  ;; (type "text")
                  ;; (value ,(match value
                  ;;           ((? number? x) (number->string x))
                  ;;           ((? symbol? sym) (symbol->string sym))
                  ;;           (str str)))

                  ;; TODO: When backspacing from char 0, commit and
                  ;; begin editing the previous node if there is
                  ;; one.  When forward deleting from the last char,
                  ;; do the same for the next node.
                  (beforeinput ,(lambda (e)
                                  (match (input-event-data e)
                                    (#f
                                     (values))
                                    (" "
                                     (prevent-default! e)
                                     (when (commit-maybe)
                                       (insert id "" (get-element-by-id (exp-id id)))))
                                    ("("
                                     (prevent-default! e)
                                     (let ((focus (commit-or-delete)))
                                       (open focus (get-element-by-id (exp-id focus)))))
                                    (")"
                                     (prevent-default! e)
                                     (let ((focus (commit-or-delete)))
                                       (jump-to-eol focus)))
                                    ;; TODO: Jump to nearest closing paren
                                    ;; (")" ...)
                                    (_ (values)))))
                  (focusout ,(lambda (e)
                               (commit-or-delete)))
                  (keydown ,(lambda (e)
                              (stop-propagation! e)
                              (match (keyboard-event-key e)
                                ("Enter"
                                 (commit-or-delete)
                                 (prevent-default! e))
                                ;; ("Escape"
                                ;;  (replace-with! new orig)
                                ;;  (focus! orig)
                                ;;  (prevent-default! e))
                                (_ (values))))))
               ,value)))
     (replace-with! orig new)
     (focus! new))))

(define (wrap id elem)
  (call-with-values (lambda () (tree-wrap *tree* id))
    (lambda (tree new)
      (let ((new-elem (shtml->dom (tree->shtml tree new))))
        (set! *tree* tree)
        (replace-with! elem new-elem)
        (focus! new-elem)))))

(define (insert id init elem)
  (call-with-values (lambda () (tree-insert-atom *tree* id init))
    (lambda (tree new)
      (let* ((parent (tree-parent tree new))
             (new-elem (shtml->dom (tree->shtml tree parent))))
        (set! *tree* tree)
        (replace-with! (get-element-by-id (exp-id parent)) new-elem)
        (edit new (get-element-by-id (exp-id new)))))))

(define (open id elem)
  (call-with-values (lambda () (tree-open *tree* id))
    (lambda (tree new)
      (let* ((parent (tree-parent tree new))
             (new-elem (shtml->dom (tree->shtml tree parent))))
        (set! *tree* tree)
        (replace-with! (get-element-by-id (exp-id parent)) new-elem)
        (focus! (get-element-by-id (exp-id new)))))))

(define (jump-to-eol id)
  (match (tree-parent *tree* id)
    (#f (values))
    (parent
     (let ((parent-node (tree-ref *tree* parent)))
       (focus!
        (get-element-by-id
         (exp-id (compound-node-eol parent-node))))))))

(define (forward-delete id elem)
  (call-with-values (lambda () (tree-forward-delete *tree* id))
    (lambda (tree focus)
      (unless (eq? tree *tree*)
        (let* ((parent (tree-parent tree focus))
               (new-elem (shtml->dom (tree->shtml tree parent))))
          (set! *tree* tree)
          (replace-with! (get-element-by-id (exp-id parent)) new-elem)))
      (focus! (get-element-by-id (exp-id focus))))))

(define (backward-delete id elem)
  (call-with-values (lambda () (tree-backward-delete *tree* id))
    (lambda (tree focus)
      (unless (eq? tree *tree*)
        (let* ((parent (tree-parent tree focus))
               (new-elem (shtml->dom (tree->shtml tree parent))))
          (set! *tree* tree)
          (replace-with! (get-element-by-id (exp-id parent)) new-elem)))
      (focus! (get-element-by-id (exp-id focus))))))

(define* (tree->shtml tree #:optional (root (tree-root tree)))
  (let lp ((id root))
    (match (tree-ref tree id)
      (($ <node:atom> _ value)
       (node-element id 'atom (list value)))
      ;; (($ <node:compound> _ 'list () eol)
      ;;  (node-element id "compound"
      ;;                (list (open-delimiter "(")
      ;;                      (node-element eol "compound"
      ;;                                    (list (close-delimiter ")"))))))
      ;; TODO: To properly highlight parens, keep track of indentation
      ;; level while rendering and add a CSS class for each unique
      ;; color.
      (($ <node:compound> _ 'list children eol)
       (node-element
        id 'compound
        `(,(open-delimiter "(")
          ,@(match children
              (() '())
              ((first . rest)
               (cons (lp first)
                     (let lp2 ((rest rest))
                       (match rest
                         (() '())
                         ;; We use whitespace here for the sake of copying
                         ;; code as plain text.
                         ((child . rest)
                          (cons* " " (lp child) (lp2 rest))))))))
          ,(close-delimiter eol ")")))))))

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
          ;; ,(map (lambda (id)
          ;;         (tree->shtml *tree* id))
          ;;       (compound-node-children
          ;;        (tree-ref *tree* (tree-root *tree*))))
          ,(tree->shtml *tree*)))))
