(use-modules (dom)
             (editor)
             (fibers scheduler)
             (hoot ffi)
             (ice-9 match))

;; TODO: Add indenting rules.
(define %special-forms
  '(and
    call-with-prompt
    call-with-values
    define
    define*
    define-module
    define-record-type
    if
    lambda
    let
    let*
    letrec
    letrec*
    match
    or
    set!
    unless
    when))
(define (special-form? x) (memq x %special-forms))

(define *tree* (make-empty-tree))
(define *undo-stack* '())
(define *redo-stack* '())

(define (set-tree! tree)
  (set! *undo-stack* (cons *tree* *undo-stack*))
  (set! *tree* tree)
  (set! *redo-stack* '()))

(define (undo-tree!)
  (match *undo-stack*
    (() *tree*)
    ((tree . rest)
     (set! *redo-stack* (cons *tree* *redo-stack*))
     (set! *tree* tree)
     (set! *undo-stack* rest)
     tree)))

(define (redo-tree!)
  (match *redo-stack*
    (() *tree*)
    ((tree . rest)
     (set! *undo-stack* (cons *tree* *undo-stack*))
     (set! *tree* tree)
     (set! *redo-stack* rest)
     tree)))

(define (reset! old new id)
  (unless (eq? old new)
    (replace-with!
     (element-first-child (get-element-by-id "editor"))
     (shtml->dom (tree->shtml new)))
    (let lp ((id id))
      (if (tree-ref new id)
          (focus! (get-element-by-id (exp-id id)))
          (lp (or (tree-prev old id) (tree-root new)))))))

(define (undo! id)
  (let* ((old *tree*)
         (new (undo-tree!)))
    (reset! old new id)))

(define (redo! id)
  (let* ((old *tree*)
         (new (redo-tree!)))
    (reset! old new id)))

(define (exp-id id)
  (string-append "exp-" (number->string id)))

(define (focus-maybe id)
  (when id
    (focus! (get-element-by-id (exp-id id)))))

(define (edit-begin! elem)
  (set-attribute! elem "contenteditable" "plaintext-only"))

(define (edit-end! elem)
  (set-attribute! elem "contenteditable" "false"))

(define (open-delimiter str)
  `(span (@ (class "editor-delimiter editor-delimiter-open")) ,str))

(define (close-delimiter id str)
  (noneditable-node-shtml id "editor-delimiter editor-delimiter-close" (list str)))

(define (noneditable-node-shtml id class contents)
  `(span (@ (id ,(exp-id id))
            (class ,(string-append "editor-node " class))
            (tabindex "0")
            (click ,(lambda (e) (focus! (event-target e))))
            (keydown ,(lambda (e)
                        (stop-propagation! e)
                        (match (pk 'key (keyboard-event-key e))
                          ("ArrowLeft"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               (jump-to-prev id)))
                          ("ArrowRight"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-next-sexp id)
                               (jump-to-next id)))
                          ("ArrowUp"
                           (jump-to-prev-sexp id))
                          ("ArrowDown"
                           (jump-to-next-sexp id))
                          ;; ("Enter"
                          ;;  (insert id (string->symbol "")))
                          ("("
                           (let ((elem (event-target e)))
                             (if (keyboard-event-alt-key? e)
                                 (wrap id elem)
                                 (open id elem))))
                          (")"
                           (jump-to-eol id))
                          (" "
                           (prevent-default! e)
                           (insert id '#{}#))
                          ("\""
                           (prevent-default! e)
                           (insert id ""))
                          ("z"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (undo! id)))
                          ("y"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo! id)))
                          ("Delete"
                           (if (keyboard-event-ctrl-key? e)
                               (forward-delete id (event-target e))
                               (jump-to-next id)))
                          ("Backspace"
                           (if (keyboard-event-ctrl-key? e)
                               (backward-delete id (event-target e))
                               (jump-to-prev id)))
                          ((or "Ctrl" "Alt" "Shift")
                           (values))
                          (key
                           (when (and (= (string-length key) 1)
                                      (not (keyboard-event-ctrl-key? e))
                                      (not (keyboard-event-alt-key? e)))
                             (prevent-default! e)
                             (insert id (string->symbol key))))))))
         ,@contents))

(define (element-inner-text/trimmed elem)
  (string-trim-both (element-inner-text elem)))

(define (string-node-shtml id value)
  (let ((elem-id (exp-id id)))
    `(span (@ (id ,elem-id)
              (class "editor-node editor-string")
              (tabindex "0")
              (keydown
               ,(lambda (e)
                  ;; Move into contenteditable child node.
                  (define (jump-to-inner)
                    (focus! (element-next-sibling
                             (element-first-child
                              (event-target e)))))
                  (stop-propagation! e)
                  (match (pk 'key (keyboard-event-key e))
                    ("ArrowLeft"
                     (if (keyboard-event-ctrl-key? e)
                         (jump-to-prev-sexp id)
                         (jump-to-prev id)))
                    ("ArrowRight"
                     (if (keyboard-event-ctrl-key? e)
                         (jump-to-next-sexp id)
                         (jump-to-inner)))
                    ("ArrowUp"
                     (jump-to-prev-sexp id))
                    ("ArrowDown"
                     (jump-to-next-sexp id))
                    ("("
                     (let ((elem (event-target e)))
                       (if (keyboard-event-alt-key? e)
                           (wrap id elem)
                           (open id elem))))
                    (")"
                     (jump-to-eol id))
                    ("\""
                     (prevent-default! e)
                     ;; Jump to closing delimiter.
                     (focus!
                      (element-next-sibling
                       (element-next-sibling
                        (element-first-child
                         (event-target e))))))
                    ("z"
                     (when (keyboard-event-ctrl-key? e)
                       (prevent-default! e)
                       (undo! id)))
                    ("y"
                     (when (keyboard-event-ctrl-key? e)
                       (prevent-default! e)
                       (redo! id)))
                    ("Delete"
                     (if (keyboard-event-ctrl-key? e)
                         (forward-delete id (event-target e))
                         (jump-to-inner)))
                    ("Backspace"
                     (when (keyboard-event-ctrl-key? e)
                       (backward-delete id (event-target e))
                       (jump-to-prev id)))
                    (key
                     (when (and (= (string-length key) 1)
                                (not (keyboard-event-ctrl-key? e))
                                (not (keyboard-event-alt-key? e)))
                       (jump-to-inner)))))))
           (span (@ (class "editor-string-delimiter")) "\"")
           (span (@ (class "editor-string-inner")
                    (tabindex "0")
                    (contenteditable "false")
                    (focusin ,(lambda (e)
                                (edit-begin! (event-target e))))
                    (focusout ,(lambda (e)
                                 (edit-end! (event-target e))))
                    (beforeinput
                     ,(lambda (e)
                        (let ((elem (event-target e)))
                          (match (pk 'beforeinput (input-event-type e))
                            ("insertText"
                             (match (input-event-data e)
                               ("\""
                                ;; Unless there is a \ before the
                                ;; caret, a " jumps to the closing
                                ;; delimiter.
                                (match (input-event-target-ranges e)
                                  ((range)
                                   (let ((start (range-start-offset range)))
                                     (match (string-ref (element-inner-text
                                                         (event-target e))
                                                        (1- start))
                                       (#\\ (values))
                                       (_
                                        (prevent-default! e)
                                        (focus!
                                         (element-next-sibling
                                          (event-target e)))))))))
                               (_ (values))))
                            ((or "insertLineBreak"
                                 "insertParagraph"
                                 "insertOrderedList"
                                 "insertUnorderedList"
                                 "insertHorizontalRule")
                             (prevent-default! e))
                            ((or "deleteContentForward"
                                 "deleteWordForward"
                                 "deleteSoftLineForward"
                                 "deleteHardLineForward")
                             (when (string-null? (element-inner-text elem))
                               (forward-delete id elem)))
                            ((or "deleteContentBackward"
                                 "deleteWordBackward"
                                 "deleteSoftLineBackward"
                                 "defleteHardLineBackward")
                             (when (string-null? (element-inner-text elem))
                               (backward-delete (tree-next *tree* id) elem)))
                            (_ (values))))))
                    ;; Synchronize element text with the relevant atom
                    ;; node.
                    (input ,(lambda (e)
                              (let ((new (element-inner-text (event-target e))))
                                (unless (equal? value new)
                                  (set! value new)
                                  (set-tree! (tree-set-atom *tree* id new))))))
                    (keydown
                     ,(lambda (e)
                        (stop-propagation! e)
                        (match (pk 'key (keyboard-event-key e))
                          ("ArrowLeft"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               ;; TODO: Jump to previous node if
                               ;; caret is at first char.
                               #f))
                          ("ArrowRight"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-next-sexp id)
                               ;; TODO: Jump to next node if caret
                               ;; is at last char.
                               #f))
                          ("ArrowUp"
                           (jump-to-prev-sexp id))
                          ("ArrowDown"
                           (jump-to-next-sexp id))
                          ("z"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (undo! id)))
                          ("y"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo! id)))
                          ;; ("Enter"
                          ;;  (prevent-default! e)
                          ;;  (focus!
                          ;;   (get-element-by-id
                          ;;    (exp-id (commit-or-delete id (event-target e))))))
                          ;; ("Delete"
                          ;;  (when (keyboard-event-ctrl-key? e)
                          ;;    (forward-delete id (event-target e))))
                          ;; ("Backspace"
                          ;;  (when (keyboard-event-alt-key? e)
                          ;;    (backward-delete id (event-target e))))
                          (_ (values))))))
                 ,value)
           (span (@ (class "editor-string-delimiter")
                    (tabindex "0")
                    (keydown
                     ,(lambda (e)
                        (stop-propagation! e)
                        (match (pk 'key (keyboard-event-key e))
                          ("ArrowLeft"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               ;; Jump to opening delimiter.
                               (focus! (element-parent (event-target e)))))
                          ("ArrowRight"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-next-sexp id)
                               (jump-to-next id)))
                          ("ArrowUp"
                           (jump-to-prev-sexp id))
                          ("ArrowDown"
                           (jump-to-next-sexp id))
                          ("("
                           (let ((elem (event-target e)))
                             (if (keyboard-event-alt-key? e)
                                 (wrap id elem)
                                 (open id elem))))
                          (")"
                           (jump-to-eol id))
                          (" "
                           (prevent-default! e)
                           (insert id '#{}#))
                          ("\""
                           (prevent-default! e)
                           (insert id ""))
                          ("z"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (undo! id)))
                          ("y"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo! id)))
                          ("Delete"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (forward-delete id (event-target e)))
                            (else
                             (prevent-default! e)
                             (jump-to-next id))))
                          ("Backspace"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (backward-delete id (event-target e)))
                            (else
                             (prevent-default! e)
                             (jump-to-prev id))))
                          (key
                           (when (and (= (string-length key) 1)
                                      (not (keyboard-event-ctrl-key? e))
                                      (not (keyboard-event-alt-key? e)))
                             (prevent-default! e)
                             (insert id (string->symbol key))))))))
                 "\""))))

;; non-string atoms only
(define (atom-node-shtml id value)
  (let ((elem-id (exp-id id)))
    (define (empty? e)
      (string-null?
       (element-inner-text/trimmed
        (event-target e))))
    (define (delete-maybe e)
      (if (empty? e)
          (let* ((prev (tree-prev *tree* id))
                 (tree (tree-delete *tree* id)))
            (set-tree! tree)
            (remove! (event-target e))
            prev)
          id))
    (define (node-classes)
      (string-append
       "editor-node editor-atom "
       (match value
         ((? symbol?)
          ;; TODO: Only highlight special forms
          ;; that are at the head of a list.
          (if (special-form? value)
              "editor-special-form"
              "editor-symbol"))
         ((? keyword?) "editor-keyword")
         ((? number?) "editor-number")
         ((or #t #f) "editor-boolean"))))
    `(span (@ (id ,elem-id)
              (class ,(node-classes))
              (contenteditable "false")
              (tabindex "0")
              (focusin ,(lambda (e)
                          (edit-begin! (event-target e))))
              (focusout ,(lambda (e)
                           (edit-end! (event-target e))
                           (delete-maybe e)))
              (beforeinput
               ,(lambda (e)
                  (let ((elem (event-target e))
                        (ranges (input-event-target-ranges e)))
                    (match (pk 'beforeinput (input-event-type e))
                      ("insertText"
                       (match (input-event-data e)
                         ("\""
                          (prevent-default! e)
                          (let ((focus (delete-maybe e)))
                            (insert focus "")))
                         (_ (values))))
                      ((or "insertLineBreak"
                           "insertParagraph"
                           "insertOrderedList"
                           "insertUnorderedList"
                           "insertHorizontalRule")
                       (prevent-default! e))
                      ((or "deleteContentForward"
                           "deleteWordForward"
                           "deleteSoftLineForward"
                           "deleteHardLineForward")
                       (when (string-null? (element-inner-text/trimmed elem))
                         (forward-delete id elem)))
                      ((or "deleteContentBackward"
                           "deleteWordBackward"
                           "deleteSoftLineBackward"
                           "defleteHardLineBackward")
                       (when (string-null? (element-inner-text/trimmed elem))
                         (backward-delete (tree-next *tree* id) elem)))
                      (_ (values))))))
              ;; Synchronize element text with the relevant atom node.
              (input
               ,(lambda (e)
                  ;; TODO: Add some indicator when the text fails to
                  ;; parse.
                  (let ((new (with-exception-handler (lambda (exn) value)
                               (lambda ()
                                 (let ((str (element-inner-text/trimmed
                                             (event-target e))))
                                   (match (call-with-input-string str read)
                                     ((? eof-object?) value)
                                     (x x))))
                               #:unwind? #t)))
                    (unless (equal? value new)
                      (set! value new)
                      (set-tree! (tree-set-atom *tree* id new))
                      (set-attribute! (event-target e) "class"
                                      (node-classes))))))
              (keydown
               ,(lambda (e)
                  (stop-propagation! e)
                  (match (pk 'key (keyboard-event-key e))
                    ("ArrowLeft"
                     (if (keyboard-event-ctrl-key? e)
                         (jump-to-prev-sexp id)
                         ;; TODO: Jump to previous node if
                         ;; caret is at first char.
                         #f))
                    ("ArrowRight"
                     (if (keyboard-event-ctrl-key? e)
                         (jump-to-next-sexp id)
                         ;; TODO: Jump to next node if caret
                         ;; is at last char.
                         #f))
                    ("ArrowUp"
                     (let ((pos (delete-maybe e)))
                       (jump-to-prev-sexp pos)))
                    ("ArrowDown"
                     (let ((pos (delete-maybe e)))
                       (jump-to-next-sexp pos)))
                    ("("
                     (let* ((pos (delete-maybe e))
                            (elem (get-element-by-id (exp-id pos))))
                       (if (and (keyboard-event-alt-key? e) (eqv? pos id))
                           (wrap id elem)
                           (open pos elem))))
                    (")"
                     (let ((pos (delete-maybe e)))
                       (jump-to-eol pos)))
                    (" "
                     (prevent-default! e)
                     (unless (empty? e)
                       (insert id '#{}#)))
                    ("z"
                     (when (keyboard-event-ctrl-key? e)
                       (prevent-default! e)
                       (undo! id)))
                    ("y"
                     (when (keyboard-event-ctrl-key? e)
                       (prevent-default! e)
                       (redo! id)))
                    ("Delete"
                     (when (keyboard-event-ctrl-key? e)
                       (forward-delete id (event-target e))))
                    ("Backspace"
                     (when (keyboard-event-alt-key? e)
                       (backward-delete id (event-target e))))
                    (_ (values))))))
           ,value)))

(define (wrap id elem)
  (call-with-values (lambda () (tree-wrap *tree* id))
    (lambda (tree new)
      (let ((new-elem (shtml->dom (tree->shtml tree new))))
        (set-tree! tree)
        (replace-with! elem new-elem)
        (focus! new-elem)))))

(define (insert id init)
  (call-with-values (lambda () (tree-insert-atom *tree* id init))
    (lambda (tree new)
      (let* ((parent (tree-parent tree new))
             (parent-elem (get-element-by-id (exp-id parent)))
             (new-parent-elem (shtml->dom (tree->shtml tree parent))))
        (set-tree! tree)
        (replace-with! parent-elem new-parent-elem)
        (let ((focus-elem (get-element-by-id (exp-id new))))
          (focus! focus-elem)
          (set-selection-position! (selection) focus-elem 1))))))

(define (open id elem)
  (call-with-values (lambda () (tree-open *tree* id))
    (lambda (tree new)
      (let* ((parent (tree-parent tree new))
             (new-elem (shtml->dom (tree->shtml tree parent))))
        (set-tree! tree)
        (replace-with! (get-element-by-id (exp-id parent)) new-elem)
        (focus! (get-element-by-id (exp-id new)))))))

(define (jump-to-prev id)
  (focus-maybe (tree-prev *tree* id)))

(define (jump-to-next id)
  (focus-maybe (tree-next *tree* id)))

(define (jump-to-prev-sexp id)
  (focus-maybe (tree-prev-sexp *tree* id)))

(define (jump-to-next-sexp id)
  (focus-maybe (tree-next-sexp *tree* id)))

(define (jump-to-eol id)
  (match (tree-next-eol *tree* id)
    (#f (values))
    (focus
     (focus! (get-element-by-id (exp-id focus))))))

(define (forward-delete id elem)
  (call-with-values (lambda () (tree-forward-delete *tree* id))
    (lambda (tree focus)
      (unless (eq? tree *tree*)
        (let* ((parent (tree-parent tree focus))
               (new-elem (shtml->dom (tree->shtml tree parent))))
          (set-tree! tree)
          (replace-with! (get-element-by-id (exp-id parent)) new-elem)))
      (focus! (get-element-by-id (exp-id focus))))))

(define (backward-delete id elem)
  (call-with-values (lambda () (tree-backward-delete *tree* id))
    (lambda (tree focus)
      (unless (eq? tree *tree*)
        (let* ((parent (tree-parent tree focus))
               (new-elem (shtml->dom (tree->shtml tree parent))))
          (set-tree! tree)
          (replace-with! (get-element-by-id (exp-id parent)) new-elem)))
      (focus! (get-element-by-id (exp-id focus))))))

(define* (tree->shtml tree #:optional (root (tree-root tree)))
  (let lp ((id root))
    (match (tree-ref tree id)
      (($ <node:atom> _ (? string? value))
       (string-node-shtml id value))
      (($ <node:atom> _ value)
       (atom-node-shtml id value))
      ;; TODO: To properly highlight parens, keep track of indentation
      ;; level while rendering and add a CSS class for each unique
      ;; color.
      (($ <node:compound> _ 'list children eol)
       (noneditable-node-shtml
        id "editor-compound"
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

(append-child!
 (document-body)
 (shtml->dom
  `(article
    (h1 "Structural editor demo")
    (main (@ (id "editor") (class "editor"))
          ,(tree->shtml *tree*)))))
