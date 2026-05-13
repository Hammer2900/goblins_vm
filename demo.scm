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
(define *selection* (list (tree-root *tree*) 0))
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

(define (highlight-class i)
  (string-append "editor-highlight-" (number->string (modulo i 6))))

(define* (open-delimiter id depth #:optional (str "("))
  (paren-node-shtml id
                    (string-append "editor-delimiter editor-delimiter-open "
                                   (highlight-class depth))
                    str))

(define* (close-delimiter id depth #:optional (str ")"))
  (paren-node-shtml id
                    (string-append "editor-delimiter editor-delimiter-close "
                                   (highlight-class depth))
                    str))

(define (paren-node-shtml id class str)
  `(span (@ (id ,(exp-id id))
            (class ,(string-append "editor-node " class))
            (tabindex "0")
            (click ,(lambda (e) (element-focus! (event-target e))))
            (keydown ,(lambda (e)
                        (stop-propagation! e)
                        (match (pk 'paren-key (keyboard-event-key e))
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
                             (insert id (string->sexp key))))))))
         ,str))

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
                    (element-focus!
                     (element-next-sibling
                      (element-first-child
                       (event-target e)))))
                  (stop-propagation! e)
                  (match (pk 'string-open-key (keyboard-event-key e))
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
                     (element-focus!
                      (element-next-sibling
                       (element-next-sibling
                        (element-first-child
                         (event-target e))))))
                    ("z"
                     (when (keyboard-event-ctrl-key? e)
                       (prevent-default! e)
                       (undo-tree! id)))
                    ("y"
                     (when (keyboard-event-ctrl-key? e)
                       (prevent-default! e)
                       (redo-tree! id)))
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
                                  (set-tree! (tree-set-atom *tree* id new) #f)))))
                    (keydown
                     ,(lambda (e)
                        (stop-propagation! e)
                        (match (pk 'string-inner-key (keyboard-event-key e))
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
                        (stop-propagation! e)
                        (match (pk 'string-close-key (keyboard-event-key e))
                          ("ArrowLeft"
                           (if (keyboard-event-ctrl-key? e)
                               (jump-to-prev-sexp id)
                               ;; Jump to opening delimiter.
                               (element-focus! (element-parent (event-target e)))))
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
                             (undo-tree! id)))
                          ("y"
                           (when (keyboard-event-ctrl-key? e)
                             (prevent-default! e)
                             (redo-tree! id)))
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
                             (insert id (string->sexp key))))))))
                 "\""))))

;; non-string atoms only
(define (atom-node-shtml id value operator?)
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
            (element-remove! (event-target e))
            prev)
          id))
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
         ((or #t #f) "editor-boolean"))))
    `(span (@ (id ,elem-id)
              (class ,(node-classes))
              (contenteditable "plaintext-only")
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
                       (undo-tree! id)))
                    ("y"
                     (when (keyboard-event-ctrl-key? e)
                       (prevent-default! e)
                       (redo-tree! id)))
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
      (set-tree! tree)
      (element-focus! (element-for-id new)))))

(define (insert id init)
  (call-with-values (lambda () (tree-insert-atom *tree* id init))
    (lambda (tree new)
      (set-tree! tree)
      (let ((focus-elem (element-for-id new)))
        (element-focus! focus-elem)
        (set-selection-position! (selection) focus-elem 1)))))

(define (open id elem)
  (call-with-values (lambda () (tree-open *tree* id))
    (lambda (tree new)
      (set-tree! tree)
      (element-focus! (element-for-id new)))))

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
    (id (element-focus! (element-for-id id)))))

(define (forward-delete id elem)
  (call-with-values (lambda () (tree-forward-delete *tree* id))
    (lambda (tree focused)
      (set-tree! tree)
      (element-focus! (element-for-id focused)))))

(define (backward-delete id elem)
  (call-with-values (lambda () (tree-backward-delete *tree* id))
    (lambda (tree focused)
      (set-tree! tree)
      (element-focus! (element-for-id focused)))))

(define* (tree->shtml tree)
  (let lp ((id (tree-root tree))
           (depth 0)
           (opens '())
           (closes '()))
    (define (open-delimiters)
      (map (match-lambda
             ((id . depth)
              (open-delimiter id depth)))
           (reverse opens)))
    (define (close-delimiters)
      (map (match-lambda
             ((id . depth)
              (close-delimiter id depth)))
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
                       ,(open-delimiter id depth)
                       ,(close-delimiter eol depth)
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
          (let-values (((symbolic? args body)
                        (match (tree-ref tree first)
                          (($ <node:atom> _ (? symbol? sym))
                           (match (special-form-arguments sym)
                             ((or #t #f)
                              (values #t rest '()))
                             (nargs
                              (let-values (((args body) (split-at rest nargs)))
                                (values #t args body)))))
                          (_ (values #f rest '())))))
            (match body
              (()
               (if symbolic?
                   ;; Lists that start with a symbol get standard
                   ;; procedure call styling where the arguments are
                   ;; aligned to the right of the operator.
                   `(span (@ (class "editor-call"))
                          ,(lp first (1+ depth) (alist-cons id depth opens) '())
                          (span (@ (class "editor-arguments"))
                                ,@(render-args args (alist-cons eol depth closes))))
                   ;; Lists that don't start with a symbol have a
                   ;; slightly different style where the arguments are
                   ;; aligned to the right of the opening parenthesis.
                   `(span (@ (class "editor-list"))
                          ,(lp first (1+ depth) (alist-cons id depth opens) '())
                          ,@(render-args args (alist-cons eol depth closes)))))
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
