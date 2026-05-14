(define-module (dom)
  #:use-module (hoot ffi)
  #:use-module (hoot hashtables)
  #:use-module (fibers promises)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:export (document-body
            get-element-by-id
            make-text-node
            make-element

            element-parent
            element-first-child
            element-last-child
            element-next-sibling
            element-previous-sibling
            element-append-child!
            element-remove!
            element-replace-with!
            element-value
            set-element-value!
            element-inner-text
            element-attribute-set!
            element-attribute-remove!
            element-focus!

            add-event-listener!
            remove-event-listener!
            prevent-default!
            stop-propagation!
            event-target
            event-current-target

            keyboard-event-key
            keyboard-event-alt-key?
            keyboard-event-ctrl-key?

            input-event-type
            input-event-data
            input-event-target-ranges

            range-start-container
            range-start-offset
            range-end-container
            range-end-offset

            selection
            selection-anchor-node
            selection-anchor-offset
            selection-focus-node
            selection-focus-offset
            selection-collapsed?
            selection-collapse!
            set-selection-base-and-extent!
            set-selection-position!

            clipboard-text

            shtml->dom
            virtual-dom-render))

(define-foreign array-length
  "array" "length"
  (ref extern) -> i32)
(define-foreign array-ref-extern
  "array" "get"
  (ref extern) i32 -> (ref null extern))
(define (array->list/extern array)
  (let ((len (array-length array)))
    (let lp ((i 0))
      (if (< i len)
          (cons (array-ref-extern array i) (lp (1+ i)))
          '()))))

(define-foreign selection
  "window" "getSelection"
  -> (ref extern))

(define-foreign document-body
  "document" "body"
  -> (ref extern))
(define-foreign get-element-by-id
  "document" "getElementById"
  (ref string) -> (ref null extern))
(define-foreign make-text-node
  "document" "createTextNode"
  (ref string) -> (ref extern))
(define-foreign make-element
  "document" "createElement"
  (ref string) -> (ref extern))

(define-foreign element-parent
  "element" "parent"
  (ref extern) -> (ref null extern))
(define-foreign element-first-child
  "element" "firstChild"
  (ref extern) -> (ref null extern))
(define-foreign element-last-child
  "element" "lastChild"
  (ref extern) -> (ref null extern))
(define-foreign element-next-sibling
  "element" "nextSibling"
  (ref extern) -> (ref null extern))
(define-foreign element-previous-sibling
  "element" "previousSibling"
  (ref extern) -> (ref null extern))
(define-foreign element-append-child!
  "element" "appendChild"
  (ref extern) (ref extern) -> (ref extern))
(define-foreign element-remove!
  "element" "remove"
  (ref extern) -> none)
(define-foreign element-replace-with!
  "element" "replaceWith"
  (ref extern) (ref extern) -> none)
(define-foreign element-value
  "element" "value"
  (ref extern) -> (ref string))
(define-foreign set-element-value!
  "element" "setValue"
  (ref extern) (ref string) -> none)
(define-foreign element-inner-text
  "element" "innerText"
  (ref extern) -> (ref string))
(define-foreign element-attribute-set!
  "element" "setAttribute"
  (ref extern) (ref string) (ref string) -> none)
(define-foreign element-attribute-remove!
  "element" "removeAttribute"
  (ref extern) (ref string) -> none)
(define-foreign element-focus!
  "element" "focus"
  (ref extern) -> none)

(define-foreign add-event-listener!
  "event" "addEventListener"
  (ref extern) (ref string) (ref extern) -> none)
(define-foreign remove-event-listener!
  "event" "removeEventListener"
  (ref extern) (ref string) (ref extern) -> none)
(define-foreign prevent-default!
  "event" "preventDefault"
  (ref extern) -> none)
(define-foreign stop-propagation!
  "event" "stopPropagation"
  (ref extern) -> none)
(define-foreign event-target
  "event" "target"
  (ref extern) -> (ref extern))
(define-foreign event-current-target
  "event" "currentTarget"
  (ref extern) -> (ref extern))

(define-foreign keyboard-event-key
  "event" "keyboardKey"
  (ref extern) -> (ref string))
(define-foreign %keyboard-event-alt-key?
  "event" "keyboardAltKey"
  (ref extern) -> i32)
(define (keyboard-event-alt-key? e)
  (eq? (%keyboard-event-alt-key? e) 1))
(define-foreign %keyboard-event-ctrl-key?
  "event" "keyboardCtrlKey"
  (ref extern) -> i32)
(define (keyboard-event-ctrl-key? e)
  (eq? (%keyboard-event-ctrl-key? e) 1))

(define-foreign input-event-type
  "event" "inputType"
  (ref extern) -> (ref string))
(define-foreign input-event-data
  "event" "inputData"
  (ref extern) -> (ref null string))
(define-foreign %input-event-target-ranges
  "event" "inputTargetRanges"
  (ref extern) -> (ref extern))
(define (input-event-target-ranges e)
  (array->list/extern (%input-event-target-ranges e)))

(define-foreign range-start-container
  "range" "startContainer"
  (ref extern) -> (ref extern))
(define-foreign range-start-offset
  "range" "startOffset"
  (ref extern) -> i32)
(define-foreign range-end-container
  "range" "endContainer"
  (ref extern) -> (ref extern))
(define-foreign range-end-offset
  "range" "endOffset"
  (ref extern) -> i32)

(define-foreign selection-anchor-node
  "selection" "anchorNode"
  (ref extern) -> (ref null extern))
(define-foreign selection-anchor-offset
  "selection" "anchorOffset"
  (ref extern) -> i32)
(define-foreign selection-focus-node
  "selection" "focusNode"
  (ref extern) -> (ref null extern))
(define-foreign selection-focus-offset
  "selection" "focusOffset"
  (ref extern) -> i32)
(define-foreign %selection-collapsed?
  "selection" "isCollapsed"
  (ref extern) -> i32)
(define (selection-collapsed? selection)
  (eq? (%selection-collapsed? selection) 1))
(define-foreign selection-collapse!
  "selection" "collapse"
  (ref extern) (ref extern) i32 -> none)
(define-foreign set-selection-position!
  "selection" "setPosition"
  (ref extern) (ref extern) i32 -> none)
(define-foreign set-selection-base-and-extent!
  "selection" "setBaseAndExtent"
  (ref extern) (ref extern) i32 (ref extern) i32 -> none)

(define-foreign %clipboard-text
  "clipboard" "readText"
  -> (ref extern))
(define (clipboard-text)
  (await (%clipboard-text)))

(define procedure->external/cached
  (let ((cache (make-weak-key-hashtable)))
    (lambda (proc)
      (or (weak-key-hashtable-ref cache proc)
          (let ((f (procedure->external proc)))
            (weak-key-hashtable-set! cache proc f)
            f)))))
(define (add-event-listener!/wrap elem name proc)
  (add-event-listener! elem name (procedure->external/cached proc)))
(define (remove-event-listener!/wrap elem name proc)
  (remove-event-listener! elem name (procedure->external/cached proc)))

(define (shtml->dom exp)
  (match exp
    ;; The simple case: a string representing a text node.
    ((? string? str)
     (make-text-node str))
    ((? char? ch)
     (make-text-node (string ch)))
    ((? number? x)
     (make-text-node (number->string x)))
    ((? symbol? sym)
     (make-text-node (symbol->string sym)))
    ((? keyword? kw)
     (make-text-node (string-append "#:" (symbol->string (keyword->symbol kw)))))
    (#t (make-text-node "#t"))
    (#f (make-text-node "#f"))
    ;; An element tree.  The first item is the HTML tag.
    (((? symbol? tag) . body)
     ;; Create a new element with the given tag.
     (let ((elem (make-element (symbol->string tag))))
       (define (add-children children)
         ;; Recursively call shtml->dom for each child node and
         ;; append it to elem.
         (for-each (lambda (child)
                     (element-append-child! elem (shtml->dom child)))
                   children))
       (match body
         ;; '@' denotes an attribute list.  Child nodes follow.
         ((('@ . attrs) . children)
          ;; Set attributes.
          (for-each (lambda (attr)
                      (match attr
                        ;; Attributes are (symbol string) tuples.
                        (((? symbol? name) (? string? val))
                         (element-attribute-set! elem
                                                 (symbol->string name)
                                                 val))
                        (((? symbol? name) (? procedure? proc))
                         (let ((name* (symbol->string name)))
                           (add-event-listener!/wrap elem name* proc)))))
                    attrs)
          (add-children children))
         ;; No attributes, just a list of child nodes.
         (children (add-children children)))
       elem))))

(define (virtual-dom-render root old new)
  (define (attrs+children exp)
    (match exp
      ((('@ . attrs) . children)
       (values attrs children))
      (children
       (values '() children))))
  (define (find-attr attrs name)
    (match attrs
      (() #f)
      ((attr . rest)
       (match attr
         ((name* val)
          (if (eq? name name*)
              val
              (find-attr rest name)))))))
  (define (update-attrs node old-attrs new-attrs)
    (for-each
     (lambda (attr)
       (match attr
         ((name val)
          (let ((name-str (symbol->string name)))
            (match (find-attr old-attrs name)
              ;; No existing attr/listener, add new one.
              (#f
               (match val
                 ((? string?)
                  (element-attribute-set! node name-str val))
                 ((? procedure?)
                  (add-event-listener!/wrap node name-str val))))
              ;; Replace old attr or listener with new.
              (old-val
               (match val
                 ((? string?)
                  (unless (equal? old-val val)
                    (element-attribute-set! node name-str val)))
                 ((? procedure?)
                  (unless (eq? old-val val)
                    (remove-event-listener!/wrap node name-str old-val)
                    (add-event-listener!/wrap node name-str val))))))))))
     new-attrs)
    ;; Delete old attrs that aren't in new.
    (for-each
     (lambda (attr)
       (match attr
         ((name val)
          (let ((name-str (symbol->string name)))
            (match (find-attr new-attrs name)
              (#f
               (match val
                 ((? string?)
                  (element-attribute-remove! node name-str))
                 ((? procedure?)
                  (remove-event-listener!/wrap node name-str val))))
              (_ #t))))))
     old-attrs))
  ;; Remove node and all siblings that follow.
  (define (remove-tail node)
    (unless (external-null? node)
      (let ((next (element-next-sibling node)))
        (element-remove! node)
        (remove-tail next))))
  (define (render-and-replace node shtml)
    (let ((node* (shtml->dom shtml)))
      (element-replace-with! node node*)
      node*))
  (let lp ((node (element-first-child root))
           (parent root)
           (old old)
           (new new))
    (match old
      (#f
       ;; It's the first render, so clear out whatever might be in the
       ;; actual DOM and render the entire tree.  No diffing
       ;; necessary.
       (let ((node* (shtml->dom new)))
         (remove-tail node)
         (element-append-child! parent node*)
         node*))
      ;; Replace text nodes with either a new text node if the text
      ;; has changed, or an element subtree if the text has been
      ;; replaced by an element.
      ((? string?)
       (if (and (string? new) (string=? old new))
           node
           (render-and-replace node new)))
      ((? char?)
       (if (and (char? new) (char=? old new))
           node
           (render-and-replace node new)))
      ((? number?)
       (if (and (number? new) (= old new))
           node
           (render-and-replace node new)))
      ((? symbol?)
       (if (and (symbol? new) (eq? old new))
           node
           (render-and-replace node new)))
      ((? keyword?)
       (if (and (keyword? new) (eq? old new))
           node
           (render-and-replace node new)))
      (#t
       (if (eq? new #t)
           node
           (render-and-replace node new)))
      (#f
       (if (eq? new #f)
           node
           (render-and-replace node new)))
      (((? symbol? old-tag) . old-rest)
       (let-values (((old-attrs old-children)
                     (attrs+children old-rest)))
         (match new
           ;; Old node was an element, but the new node is a text
           ;; node, so replace the element subtree with a text node.
           ((or (? string?) (? char?) (? number?)
                (? symbol?) (? keyword?) #t #f)
            (render-and-replace node new))
           (((? symbol? new-tag) . new-rest)
            (let-values (((new-attrs new-children)
                          (attrs+children new-rest)))
              (cond
               ;; The element tag is the same, so modify the inner
               ;; contents of the element if necessary.
               ((eq? old-tag new-tag)
                (update-attrs node old-attrs new-attrs)
                (let ((parent node))
                  (let child-lp ((node (element-first-child parent))
                                 (old old-children)
                                 (new new-children))
                    (match old
                      (()
                       ;; The old child list is empty, so diffing
                       ;; stops here.  All remaining children in the
                       ;; new list are fresh elements that need to be
                       ;; added.
                       (for-each (lambda (new)
                                   (element-append-child! parent (shtml->dom new)))
                                 new))
                      ((old-child . old-rest)
                       (match new
                         ;; The new child list is empty, so any
                         ;; remaining children in the old child list
                         ;; need to be removed, including the current
                         ;; one.
                         (() (remove-tail node))
                         ;; Recursively diff old and new child
                         ;; elements.
                         ((new-child . new-rest)
                          (let ((next (element-next-sibling node)))
                            (lp node parent old-child new-child)
                            (child-lp next old-rest new-rest))))))))
                node)
               ;; New element tag is different than the old one, so
               ;; replace the entire element subtree.
               (else
                (let ((node* (shtml->dom new)))
                  (element-replace-with! node node*)
                  node*)))))))))))
