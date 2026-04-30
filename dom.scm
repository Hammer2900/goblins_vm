(define-module (dom)
  #:use-module (hoot ffi)
  #:use-module (hoot hashtables)
  #:use-module (ice-9 match)
  #:export (document-body
            get-element-by-id
            make-text-node
            make-element
            append-child!
            remove!
            replace-with!
            element-value
            set-element-value!
            element-inner-text
            set-attribute!
            remove-attribute!
            focus!

            add-event-listener!
            remove-event-listener!
            prevent-default!
            stop-propagation!
            event-target
            keyboard-event-key
            keyboard-event-alt-key?
            keyboard-event-ctrl-key?

            shtml->dom))

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
(define-foreign append-child!
  "element" "appendChild"
  (ref extern) (ref extern) -> (ref extern))
(define-foreign remove!
  "element" "remove"
  (ref extern) -> none)
(define-foreign replace-with!
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
(define-foreign set-attribute!
  "element" "setAttribute"
  (ref extern) (ref string) (ref string) -> none)
(define-foreign remove-attribute!
  "element" "removeAttribute"
  (ref extern) (ref string) -> none)
(define-foreign focus!
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
    ((? number? x)
     (make-text-node (number->string x)))
    ((? symbol? sym)
     (make-text-node (symbol->string sym)))
    ;; An element tree.  The first item is the HTML tag.
    (((? symbol? tag) . body)
     ;; Create a new element with the given tag.
     (let ((elem (make-element (symbol->string tag))))
       (define (add-children children)
         ;; Recursively call sxml->dom for each child node and
         ;; append it to elem.
         (for-each (lambda (child)
                     (append-child! elem (shtml->dom child)))
                   children))
       (match body
         ;; '@' denotes an attribute list.  Child nodes follow.
         ((('@ . attrs) . children)
          ;; Set attributes.
          (for-each (lambda (attr)
                      (match attr
                        ;; Attributes are (symbol string) tuples.
                        (((? symbol? name) (? string? val))
                         (set-attribute! elem
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
