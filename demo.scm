(use-modules (dom)
             (editor)
             (fibers scheduler)
             (hoot ffi)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-11))

;; Global state (ew!)
(define *tree* (make-empty-tree))
(define *undo-stack* '())
(define *redo-stack* '())
(define *vdom* #f)

(define (render!)
  (let ((container (get-element-by-id "editor"))
        (vdom (tree->shtml *tree*)))
    (virtual-dom-render container *vdom* vdom)
    (set! *vdom* vdom)))

(define %special-forms
  '((and . #t)
    (call-with-prompt . 1)
    (call-with-values . 1)
    (cond . 0)
    (define . 1)
    (define* . 1)
    (define-module . 1)
    (define-record-type . 1)
    (if . #t)
    (lambda . 1)
    (let . 1)
    (let* . 1)
    (letrec . 1)
    (letrec* . 1)
    (match . 1)
    (or . #t)
    (set! . #t)
    (unless . 1)
    (when . 1)))
(define (special-form-arguments x)
  (assq-ref %special-forms x))

(define (string->sexp str)
  (call-with-input-string str read))

(define* (set-tree! tree #:optional (render? #t))
  (unless (eq? tree *tree*)
    (set! *undo-stack* (cons *tree* *undo-stack*))
    (set! *tree* tree)
    (set! *redo-stack* '())
    (when render? (render!))))

(define (undo-tree! id)
  (match *undo-stack*
    (() (values))
    ((new . rest)
     (let ((old *tree*))
       (set! *redo-stack* (cons *tree* *redo-stack*))
       (set! *tree* new)
       (set! *undo-stack* rest)
       (render!)
       (fuzzy-focus! old new id)))))

(define (redo-tree! id)
  (match *redo-stack*
    (() (values))
    ((new . rest)
     (let ((old *tree*))
       (set! *undo-stack* (cons *tree* *undo-stack*))
       (set! *tree* new)
       (set! *redo-stack* rest)
       (render!)
       (fuzzy-focus! old new id)))))

;; Focus on id, if possible, otherwise focus on something nearish to
;; it.  Used by undo/redo to move the cursor in a way that hopefully
;; isn't too surprising.
(define (fuzzy-focus! old new id)
  (let lp ((id id))
    (if (tree-ref new id)
        (element-focus! (element-for-id id))
        (lp (or (tree-prev old id) (tree-root new))))))

(define (exp-id id)
  (string-append "exp-" (number->string id)))

(define (element-for-id id)
  (get-element-by-id (exp-id id)))

(define (focus-maybe id)
  (when id
    (element-focus! (get-element-by-id (exp-id id)))))

(define (element-inner-text/trimmed elem)
  (string-trim-both (element-inner-text elem)))

(define (highlight-class i)
  (string-append "editor-highlight-" (number->string (modulo i 6))))

(define (paren-node-shtml id type depth)
  `(span (@ (id ,(exp-id id))
            (class ,(string-append "editor-node editor-delimiter "
                                   (if (eq? type 'open)
                                       "editor-delimiter-open "
                                       "editor-delimiter-close ")
                                   (highlight-class depth)))
            (tabindex "0")
            (click ,(lambda (e) (element-focus! (event-target e))))
            (paste ,(lambda (e) (paste (clipboard-text) id 0)))
            (keydown ,(lambda (e)
                        (stop-propagation! e)
                        (match (pk 'paren-key (keyboard-event-key e))
                          ("ArrowLeft"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               (jump-to-prev id)))
                          ("ArrowRight"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-next-sexp id)
                               (jump-to-next id)))
                          ("ArrowUp"
                           (if (eq? type 'open)
                               (if (keyboard-event-alt-key? e)
                                   (splice-sexp-delete-backward id)
                                   (jump-to-prev-sexp id))
                               (jump-to-prev-sexp id)))
                          ("ArrowDown"
                           (jump-to-next-sexp id))
                          ("Enter"
                           (insert id (string->symbol "")))
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
                          ("'"
                           (prevent-default! e)
                           (insert-quote id 'quote '#{}#))
                          ("`"
                           (prevent-default! e)
                           (insert-quote id 'quasiquote '#{}#))
                          (","
                           (prevent-default! e)
                           (insert-quote id 'unquote '#{}#))
                          ("z"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (undo-tree! id))
                            (else
                             (prevent-default! e)
                             (insert id 'z))))
                          ("y"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo-tree! id))
                            (else
                             (prevent-default! e)
                             (insert id 'y))))
                          ("Delete"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (forward-delete id)
                               (if (and (eq? type 'close)
                                        (tree-node-empty? *tree* id))
                                   (forward-delete id)
                                   (jump-to-next id))))
                          ("Backspace"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (backward-delete id)
                               (if (and (eq? type 'close)
                                        (tree-node-empty? *tree* id))
                                   (backward-delete id)
                                   (jump-to-prev id))))
                          ((or "Ctrl" "Alt" "Shift")
                           (values))
                          (key
                           (when (and (= (string-length key) 1)
                                      (not (keyboard-event-ctrl-key? e))
                                      (not (keyboard-event-alt-key? e)))
                             (prevent-default! e)
                             (insert id (string->sexp key))))))))
         ,(if (eq? type 'open) "(" ")")))

(define (quote-node-shtml id type)
  `(span (@ (id ,(exp-id id))
            (class "editor-node editor-quote")
            (tabindex "0")
            (click ,(lambda (e) (element-focus! (event-target e))))
            (paste ,(lambda (e) (paste (clipboard-text) id 0)))
            (keydown ,(lambda (e)
                        (stop-propagation! e)
                        (match (pk 'quote-key (keyboard-event-key e))
                          ("ArrowLeft"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               (jump-to-prev id)))
                          ("ArrowRight"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-next-sexp id)
                               ;; Skip over hidden atom node.
                               (jump-to-next (tree-next *tree* id))))
                          ("ArrowUp"
                           (if (keyboard-event-alt-key? e)
                               (splice-sexp-delete-backward id)
                               (jump-to-prev-sexp id)))
                          ("ArrowDown"
                           (jump-to-next-sexp id))
                          ("Enter"
                           (insert id (string->symbol "")))
                          ("("
                           (let ((elem (event-target e)))
                             (if (keyboard-event-alt-key? e)
                                 (wrap id elem)
                                 ;; TODO: This should shove the quoted
                                 ;; item into the parent and open a
                                 ;; new list that is within the quote.
                                 (values))))
                          (")"
                           (jump-to-eol (tree-parent *tree* id)))
                          (" "
                           (prevent-default! e)
                           (insert (tree-next-eol *tree* id) '#{}#))
                          ("\""
                           (prevent-default! e)
                           (insert (tree-next-eol *tree* id) ""))
                          ("'"
                           (prevent-default! e)
                           (set-head id 'quote))
                          ("`"
                           (prevent-default! e)
                           (set-head id 'quasiquote))
                          (","
                           (prevent-default! e)
                           (set-head id 'unquote))
                          ("@"
                           (prevent-default! e)
                           (set-head id 'unquote-splicing))
                          ("z"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (undo-tree! id))
                            (else
                             (prevent-default! e)
                             (insert (tree-next-eol *tree* id) 'z))))
                          ("y"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo-tree! id))
                            (else
                             (prevent-default! e)
                             (insert (tree-next-eol *tree* id) 'y))))
                          ("Delete"
                           (if (keyboard-event-ctrl-key? e)
                               (forward-delete id)
                               (match (compound-node-children (tree-ref *tree* id))
                                 ((_ child)
                                  (splice-sexp-delete-backward child)))))
                          ("Backspace"
                           (if (keyboard-event-ctrl-key? e)
                               (backward-delete id)
                               (jump-to-prev id)))
                          ((or "Ctrl" "Alt" "Shift")
                           (values))
                          (key
                           (when (and (= (string-length key) 1)
                                      (not (keyboard-event-ctrl-key? e))
                                      (not (keyboard-event-alt-key? e)))
                             (prevent-default! e)
                             (insert (tree-next-eol *tree* id)
                                     (string->sexp key))))))))
         ,(match type
            ('quote "'")
            ('quasiquote "`")
            ('unquote ",")
            ('unquote-splicing ",@"))))

(define (string-node-shtml id value)
  (let ((elem-id (exp-id id)))
    `(span (@ (class "editor-node editor-string"))
           (span (@ (id ,elem-id)
                    (class "editor-string-delimiter")
                    (tabindex "0")
                    (keydown
                     ,(lambda (e)
                        ;; Move into contenteditable node.
                        (define (jump-to-inner)
                          (let ((elem (element-next-sibling
                                       (event-target e))))
                            (element-focus! elem)
                            (set-selection-base-and-extent!
                             (selection) elem 0 elem 0)))
                        (stop-propagation! e)
                        (match (pk 'string-open-key (keyboard-event-key e))
                          ("ArrowLeft"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               (jump-to-prev id)))
                          ("ArrowRight"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-next-sexp id)
                               (jump-to-inner)))
                          ("ArrowUp"
                           (if (keyboard-event-alt-key? e)
                               (splice-sexp-delete-backward id)
                               (jump-to-prev-sexp id)))
                          ("ArrowDown"
                           (element-focus!
                            (element-next-sibling
                             (element-next-sibling
                              (event-target e)))))
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
                           (element-focus!
                            (element-next-sibling
                             (event-target e))))
                          ("z"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (undo-tree! id))
                            (else
                             (jump-to-inner))))
                          ("y"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo-tree! id))
                            (else
                             (jump-to-inner))))
                          ("Delete"
                           (if (keyboard-event-ctrl-key? e)
                               (forward-delete id)
                               (jump-to-inner)))
                          ("Backspace"
                           (when (keyboard-event-ctrl-key? e)
                             (backward-delete id)
                             (jump-to-prev id)))
                          (key
                           (when (and (= (string-length key) 1)
                                      (not (keyboard-event-ctrl-key? e))
                                      (not (keyboard-event-alt-key? e)))
                             (jump-to-inner)))))))
                 "\"")
           (span (@ (class "editor-string-inner")
                    (tabindex "0")
                    (contenteditable "plaintext-only")
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
                                        (element-focus!
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
                               (forward-delete id)))
                            ((or "deleteContentBackward"
                                 "deleteWordBackward"
                                 "deleteSoftLineBackward"
                                 "defleteHardLineBackward")
                             (when (string-null? (element-inner-text elem))
                               (backward-delete (tree-next *tree* id))))
                            (_ (values))))))
                    ;; Synchronize element text with the relevant atom
                    ;; node.
                    (input ,(lambda (e)
                              (let ((new (element-inner-text (event-target e))))
                                (unless (equal? value new)
                                  (set! value new)
                                  (set-tree! (tree-set-atom *tree* id new) #f)))))
                    (keydown
                     ,(lambda (e)
                        (stop-propagation! e)
                        (match (pk 'string-inner-key (keyboard-event-key e))
                          ("ArrowLeft"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               (let ((s (selection))
                                     (target (event-target e)))
                                 (when (and (selection-collapsed? s)
                                            (zero? (pk 'offset (selection-focus-offset s))))
                                   (prevent-default! e)
                                   (element-focus!
                                    (element-previous-sibling (event-target e)))))))
                          ("ArrowRight"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-next-sexp id)
                               (let ((s (selection))
                                     (target (event-target e)))
                                 (when (and (selection-collapsed? s)
                                            (= (selection-focus-offset s)
                                               (string-length
                                                (element-inner-text target))))
                                   (prevent-default! e)
                                   (element-focus!
                                    (element-next-sibling target))))))
                          ("ArrowUp"
                           (element-focus!
                            (element-previous-sibling (event-target e))))
                          ("ArrowDown"
                           (element-focus!
                            (element-next-sibling (event-target e))))
                          ("z"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (undo-tree! id)))
                          ("y"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo-tree! id)))
                          (_ (values))))))
                 ,value)
           (span (@ (class "editor-string-delimiter")
                    (tabindex "0")
                    (keydown
                     ,(lambda (e)
                        (define (jump-to-inner)
                          (let ((elem (element-previous-sibling
                                       (event-target e))))
                            (element-focus! elem)
                            (set-selection-base-and-extent!
                             (selection) elem 1 elem 1)))
                        (stop-propagation! e)
                        (match (pk 'string-close-key (keyboard-event-key e))
                          ("ArrowLeft"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               (jump-to-inner)))
                          ("ArrowRight"
                           (prevent-default! e)
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-next-sexp id)
                               (jump-to-next id)))
                          ("ArrowUp"
                           (element-focus!
                            (element-previous-sibling
                             (element-previous-sibling
                              (event-target e)))))
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
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (undo-tree! id))
                            (else
                             (prevent-default! e)
                             (insert id 'z))))
                          ("y"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo-tree! id))
                            (else
                             (prevent-default! e)
                             (insert id 'y))))
                          ("Delete"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (forward-delete id))
                            (else
                             (prevent-default! e)
                             (jump-to-next id))))
                          ("Backspace"
                           (cond
                            ((keyboard-event-ctrl-key? e)
                             (backward-delete id))
                            (else
                             (prevent-default! e)
                             (jump-to-prev id))))
                          (key
                           (when (and (= (string-length key) 1)
                                      (not (keyboard-event-ctrl-key? e))
                                      (not (keyboard-event-alt-key? e)))
                             (prevent-default! e)
                             (insert id (string->sexp key))))))))
                 "\""))))

;; non-string atoms only
(define (atom-node-shtml id value operator?)
  (let ((elem-id (exp-id id)))
    (define (empty? e)
      (string-null?
       (element-inner-text/trimmed
        (event-target e))))
    (define (caret-at-beginning?)
      (let ((s (selection)))
        (and (selection-collapsed? s)
             (zero? (selection-focus-offset s)))))
    (define (delete-maybe e)
      (if (empty? e)
          (let* ((prev (tree-prev *tree* id))
                     (tree (tree-delete *tree* id)))
                (set-tree! tree)
                prev)
          ;; ;; When quoted, delete the enclosing quote, too.
          ;; (if (tree-quoted? *tree* id)
          ;;     (let* ((parent (tree-parent *tree* id))
          ;;            (prev (tree-prev *tree* parent))
          ;;            (tree (tree-delete *tree* parent)))
          ;;       (set-tree! tree)
          ;;       prev)
          ;;     (let* ((prev (tree-prev *tree* id))
          ;;            (tree (tree-delete *tree* id)))
          ;;       (set-tree! tree)
          ;;       prev))
          id))
    (define (wrap-or-insert-quote e sym)
      (if (caret-at-beginning?)
          (wrap-quote id sym)
          (let ((focus (delete-maybe e)))
            (insert-quote focus sym '#{}#))))
    (define (node-classes)
      (string-append
       "editor-node editor-atom "
       (match value
         ((? symbol?)
          (if (and operator? (special-form-arguments value))
              "editor-special"
              "editor-symbol"))
         ((? keyword?) "editor-keyword")
         ((? number?) "editor-number")
         ((? char?) "editor-char")
         ((or #t #f) "editor-boolean"))))
    `(span (@ (id ,elem-id)
              (class ,(node-classes))
              (contenteditable "plaintext-only")
              ;; (spellcheck "false")
              (tabindex "0")
              (focusout ,(lambda (e) (delete-maybe e)))
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
                         ("'"
                          (prevent-default! e)
                          (wrap-or-insert-quote e 'quote))
                         ("`"
                          (prevent-default! e)
                          (wrap-or-insert-quote e 'quasiquote))
                         (","
                          (prevent-default! e)
                          (wrap-or-insert-quote e 'unquote))
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
                         (forward-delete id)))
                      ((or "deleteContentBackward"
                           "deleteWordBackward"
                           "deleteSoftLineBackward"
                           "defleteHardLineBackward")
                       (when (string-null? (element-inner-text/trimmed elem))
                         (backward-delete (tree-next *tree* id))))
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
                      (set-tree! (tree-set-atom *tree* id new) #f)
                      (element-attribute-set! (event-target e) "class"
                                              (node-classes))))))
              (keydown
               ,(lambda (e)
                  (stop-propagation! e)
                  (match (pk 'atom-key (keyboard-event-key e))
                    ("ArrowLeft"
                     (if (keyboard-event-ctrl-key? e)
                         (jump-to-prev-sexp id)
                         (when (caret-at-beginning?)
                           (prevent-default! e)
                           (jump-to-prev id))))
                    ("ArrowRight"
                     (if (keyboard-event-ctrl-key? e)
                         (jump-to-next-sexp id)
                         (let ((s (selection)))
                           (when (and (selection-collapsed? s)
                                      (= (selection-focus-offset s)
                                         (string-length
                                          (element-inner-text
                                           (event-target e)))))
                             (prevent-default! e)
                             (jump-to-next id)))))
                    ("ArrowUp"
                     (let ((pos (delete-maybe e)))
                       (if (keyboard-event-alt-key? e)
                           (splice-sexp-delete-backward id)
                           (jump-to-prev-sexp pos))))
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
                       (undo-tree! id)))
                    ("y"
                     (when (keyboard-event-ctrl-key? e)
                       (prevent-default! e)
                       (redo-tree! id)))
                    ("Delete"
                     (when (keyboard-event-ctrl-key? e)
                       (forward-delete id)))
                    ("Backspace"
                     (when (keyboard-event-alt-key? e)
                       (backward-delete id)))
                    (_ (values))))))
           ,value)))

(define (wrap id elem)
  (let-values (((tree new) (tree-wrap *tree* id)))
    (set-tree! tree)
    (element-focus! (element-for-id new))))

(define (wrap-quote id sym)
  (let*-values (((tree lst) (tree-wrap *tree* id))
                ((tree atom) (tree-insert-atom tree lst sym)))
    (set-tree! tree)
    (element-focus! (element-for-id id))))

(define (insert id init)
  ;; If the node is quoted then insert the element *after* the parent.
  (if (and (not (compound-node? (tree-ref *tree* id)))
           (tree-quoted? *tree* id))
      (insert (compound-node-eol (tree-ref-parent *tree* id)) init)
      (let-values (((tree new) (tree-insert-atom *tree* id init)))
        (set-tree! tree)
        (let ((focus-elem (element-for-id new)))
          (element-focus! focus-elem)
          (set-selection-position! (selection) focus-elem 1)))))

(define (insert-quote id sym init)
  (if (tree-quoted? *tree* id)
      (insert-quote (compound-node-eol (tree-ref-parent *tree* id)) sym init)
      (let*-values (((tree atom) (tree-insert-atom *tree* id init))
                    ((tree lst) (tree-wrap tree atom))
                    ((tree quo) (tree-insert-atom tree lst sym)))
        (set-tree! tree)
        (let ((focus-elem (element-for-id atom)))
          (element-focus! focus-elem)
          (set-selection-position! (selection) focus-elem 1)))))

(define (open id elem)
  (call-with-values (lambda () (tree-open *tree* id))
    (lambda (tree new)
      (set-tree! tree)
      (element-focus! (element-for-id new)))))

(define (jump-to-prev id)
  (match (tree-prev *tree* id)
    (#f (values))
    (prev
     (let ((elem (element-for-id prev)))
       ;; Skip hidden nodes.
       (if (external-null? elem)
           (jump-to-prev prev)
           (match (tree-ref *tree* prev)
             ;; Move to the closing quote of a string atom node.
             ((? string-node?)
              (element-focus!
               (element-next-sibling
                (element-next-sibling elem))))
             ;; Move to the last character of a non-string atom node.
             ((? atom-node?)
              (element-focus! elem)
              (set-selection-base-and-extent!
               (selection) elem 1 elem 1))
             (_ (element-focus! elem))))))))

(define (jump-to-next id)
  (match (tree-next *tree* id)
    (#f (values))
    (next
     (let ((elem (element-for-id next)))
       ;; Skip hidden nodes
       (if (external-null? elem)
           (jump-to-next next)
           (element-focus! elem))))))

(define (jump-to-prev-sexp id)
  (match (tree-prev-sexp *tree* id)
    (#f (values))
    (prev
     (let ((elem (element-for-id prev)))
       ;; Skip hidden nodes
       (if (external-null? elem)
           (jump-to-prev-sexp prev)
           (element-focus! elem))))))

(define (jump-to-next-sexp id)
  (match (tree-next-sexp *tree* id)
    (#f (values))
    (next
     (let ((elem (element-for-id next)))
       ;; Skip hidden nodes
       (if (external-null? elem)
           (jump-to-next-sexp next)
           (element-focus! elem))))))

(define (jump-to-eol id)
  (match (tree-next-eol *tree* id)
    (#f (values))
    (id (element-focus! (element-for-id id)))))

(define (forward-delete id)
  (call-with-values (lambda () (tree-forward-delete *tree* id))
    (lambda (tree focused)
      (set-tree! tree)
      (element-focus! (element-for-id focused)))))

(define (backward-delete id)
  (call-with-values (lambda () (tree-backward-delete *tree* id))
    (lambda (tree focused)
      (set-tree! tree)
      (element-focus! (element-for-id focused)))))

(define (set-head id x)
  (set-tree! (tree-set-atom *tree* (tree-first-child *tree* id) x)))

(define (splice-sexp-delete-backward id)
  (set-tree! (tree-splice-sexp-delete-backward *tree* id))
  (element-focus! (element-for-id id)))

;; The general idea with pasting is that we parse the pasted text and
;; before editing operations as if it were the user entering in each
;; character.
(define (paste str id offset)
  ;; Attempt to accept garbage.
  (define (string->sexp* str)
    (or (false-if-exception (string->sexp str))
        (string->symbol str)))
  (define (consume-whitespace port)
    (match (peek-char port)
      ((or #\space #\tab #\newline)
       (read-char port)
       (consume-whitespace port))
      (_ (values))))
  (define (paste-quoted port tree id offset name)
    (let*-values (((tree lst) (tree-open tree id))
                  ((tree atom) (tree-insert-atom tree lst name))
                  ((tree last) (paste* port tree atom #f)))
      (values tree lst)))
  (define (paste* port tree id offset)
    (consume-whitespace port)
    (match (peek-char port)
      ((? eof-object?)
       (values tree id))
      ((or #\( #\[)
       (read-char port)
       (let*-values (((tree lst) (tree-open tree id)))
         (let lp ((tree tree) (cur lst))
           (match (peek-char port)
             ((or (? eof-object?) #\) #\])
              (read-char port)
              (values tree lst))
             (_
              (let-values (((tree id) (paste* port tree cur #f)))
                (match (tree-ref tree id)
                  ((? compound-node? node)
                   (lp tree (compound-node-eol node)))
                  (_ (lp tree id)))))))))
      (#\'
       (read-char port)
       (paste-quoted port tree id #f 'quote))
      (#\`
       (read-char port)
       (paste-quoted port tree id #f 'quasiquote))
      (#\,
       (read-char port)
       (match (peek-char port)
         (#\@
          (read-char port)
          (paste-quoted port tree id #f 'unquote-splicing))
         (_ (paste-quoted port tree id #f 'unquote))))
      (#\"
       (read-char port)
       (let ((str (list->string
                   (let lp ()
                     (match (peek-char port)
                       ((? eof-object?)
                        '())
                       (#\\
                        (read-char port)
                        (match (read-char port)
                          ((? eof-object?)
                           '())
                          (ch
                           (cons ch (lp)))))
                       (#\"
                        (read-char port)
                        '())
                       (ch
                        (cons (read-char port) (lp))))))))
         (tree-insert-atom tree id str)))
      ;; FIXME: Support comments.
      (#\;
       (read-char port)
       (let lp ()
         (match (peek-char port)
           ((? eof-object?)
            tree)
           (#\newline
            (paste* port tree id #f))
           (_
            (read-char port)
            (lp)))))
      (_
       (let ((str (list->string
                   (cons (read-char port)
                         (let lp ()
                           (match (peek-char port)
                             ((or (? eof-object?)
                                  #\( #\) #\"
                                  #\space #\tab #\newline)
                              '())
                             (_ (cons (read-char port) (lp)))))))))
         ;; If the paste is starting in the middle of an atom node,
         ;; insert the string at the caret position.
         (if offset
             (match (tree-ref tree id)
               (($ <node:atom> _ value)
                (let* ((str (format #f "~a" value))
                       (str* (string-append
                              (substring str 0 offset)
                              str
                              (substring str offset)))
                       (value* (call-with-input-string str* read))
                       (tree (tree-set-atom tree id value*)))
                  (values tree id))))
             (let ((value (call-with-input-string str read)))
               (tree-insert-atom tree id value)))))))
  (call-with-input-string str
    (lambda (port)
      ;; The entire paste operation is a single transaction so it can
      ;; be undone with one key press.
      (let lp ((tree *tree*) (cur id))
        (match (peek-char port)
          ((? eof-object?)
           (pk 'done)
           (set-tree! tree)
           ;; TODO: Move caret back to original offset.
           (element-focus! (element-for-id id)))
          (_
           (pk 'paste)
           (let-values (((tree next) (paste* port *tree* cur offset)))
             (pk 'next next)
             (lp tree next))))))))

(define* (tree->shtml tree)
  (define (quote? x)
    (memq x '(quote quasiquote unquote unquote-splicing)))
  (let lp ((id (tree-root tree))
           (depth 0)
           (opens '())
           (closes '()))
    (define (open-delimiters)
      (map (match-lambda
             ((id . (? symbol? sym))
              (quote-node-shtml id sym))
             ((id . depth)
              (paren-node-shtml id 'open depth)))
           (reverse opens)))
    (define (close-delimiters)
      (map (match-lambda
             ((id . depth)
              (paren-node-shtml id 'close depth)))
           closes))
    (define (render-args args closes)
      (match args
        (() '())
        ((last)
         (list (lp last (1+ depth) '() closes)))
        ((middle . args)
         ;; We use whitespace here for the sake of copying
         ;; code as plain text out of the browser.
         (cons* " "
                (lp middle (1+ depth) '() '())
                '(span (@ (class "editor-break")))
                (render-args args closes)))))
    (match (tree-ref tree id)
      (($ <node:atom> _ (? string? value))
       (let ((atom (string-node-shtml id value)))
         (if (and (null? opens) (null? closes))
             atom
             `(span (@ (class "editor-group"))
                    ,@(open-delimiters)
                    ,atom
                    ,@(close-delimiters)))))
      (($ <node:atom> _ value)
       (let ((atom (atom-node-shtml id value (not (null? opens)))))
         (if (and (null? opens) (null? closes))
             atom
             `(span (@ (class "editor-group"))
                    ,@(open-delimiters)
                    ,atom
                    ,@(close-delimiters)))))
      (($ <node:compound> _ 'list children eol)
       (match children
         ;; Some special cases for lists of length 0 or 1.  We want to
         ;; make sure all the parentheses stay "glued" together.
         (()
          `(span (@ (class "editor-list"))
                 (span (@ (class "editor-group"))
                       ,@(open-delimiters)
                       ,(paren-node-shtml id 'open depth)
                       ,(paren-node-shtml eol 'close depth)
                       ,@(close-delimiters))))
         ((child)
          `(span (@ (class "editor-list"))
                 ,(lp child (1+ depth)
                      (alist-cons id depth opens)
                      (alist-cons eol depth closes))))
         ;; Lists of 2+ items are more complicated.
         ((first . rest)
          ;; Split into arguments and body expressions so we can style
          ;; each properly to achieve the conventional Lisp code
          ;; aesthetic.  Special forms will have 0 or more arguments
          ;; and then everything else is in the body.  Standard forms
          ;; will just have arguments.
          (let-values (((sym args body)
                        (match (tree-ref tree first)
                          (($ <node:atom> _ (? symbol? sym))
                           (match (special-form-arguments sym)
                             ((or #t #f)
                              (values sym rest '()))
                             (nargs
                              (let-values (((args body) (split-at rest nargs)))
                                (values sym args body)))))
                          (_ (values #f rest '())))))
            (match body
              (()
               (if (symbol? sym)
                   (if (and (memq sym '(quote quasiquote unquote unquote-splicing))
                            (= (length rest) 1))
                       ;; Lists that start with a quotation get styled
                       ;; with the expected prefix.
                       (match rest
                         ((quoted)
                          (lp quoted depth (alist-cons id sym opens) closes)))
                       ;; Lists that start with a symbol that aren't
                       ;; quotations get standard procedure call styling
                       ;; where the arguments are aligned to the right
                       ;; of the operator.
                       `(span (@ (class "editor-call"))
                              ,(lp first (1+ depth) (alist-cons id depth opens) '())
                              (span (@ (class "editor-arguments"))
                                    ,@(render-args args (alist-cons eol depth closes)))))
                   ;; Lists that don't start with a symbol have a
                   ;; slightly different style where the arguments are
                   ;; aligned to the right of the opening parenthesis.
                   `(span (@ (class "editor-list"))
                          ,(lp first (1+ depth) (alist-cons id depth opens) '())
                          ,@(render-args args (alist-cons eol depth closes)))
                   ;; ((or 'quote 'quasiquote 'unquote 'unquote-splicing)
                   ;;  (match rest
                   ;;    (()
                   ;;     (lp first (1+ depth) (alist-cons id depth opens) '()))))
                   ))
              (_
               ;; Special forms have arguments and a body, each
               ;; indented differently.
               `(span (@ (class "editor-special-form"))
                      (span (@ (class "editor-call"))
                            ,(lp first (1+ depth) (alist-cons id depth opens) '())
                            ,@(match args
                                (() '())
                                (_
                                 `((span (@ (class "editor-arguments"))
                                         ,@(render-args args '()))))))
                      (span (@ (class "editor-break"))) ; force a row break
                      (span (@ (class "editor-body"))
                            ,@(render-args body (alist-cons eol depth closes)))))))))))))

(element-append-child!
 (document-body)
 (shtml->dom
  `(article
    (h1 "Structural editor demo")
    (main (@ (id "editor") (class "editor"))))))

(render!)
