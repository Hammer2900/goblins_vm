async function load() {
  await Scheme.load_main("demo.wasm", {
    user_imports: {
      array: {
        length: (a) => a.length,
        get: (a, i) => a[i]
      },
      window: {
        getSelection: () => window.getSelection()
      },
      document: {
        body() { return document.body; },
        getElementById: Document.prototype.getElementById.bind(document),
        createElement: Document.prototype.createElement.bind(document),
        createTextNode: Document.prototype.createTextNode.bind(document)
      },
      element: {
        parent: (elem) => elem.parentElement,
        firstChild: (elem) => elem.firstChild,
        lastChild: (elem) => elem.lastChild,
        nextSibling: (elem) => elem.nextSibling,
        previousSibling: (elem) => elem.previousSibling,
        value(elem) { return elem.value; },
        setValue(elem, value) { elem.value = value; },
        innerText: (elem) => elem.innerText,
        setAttribute(elem, name, value) { elem.setAttribute(name, value); },
        removeAttribute(elem, name) { elem.removeAttribute(name); },
        appendChild(parent, child) { return parent.appendChild(child); },
        remove(elem) { elem.remove(); },
        replaceWith(oldElem, newElem) { oldElem.replaceWith(newElem); },
        focus: (elem) => elem.focus()
      },
      event: {
        addEventListener(elem, name, f) { elem.addEventListener(name, f); },
        removeEventListener(elem, name, f) { elem.removeEventListener(name, f); },
        preventDefault: (e) => e.preventDefault(),
        stopPropagation: (e) => e.stopPropagation(),
        target: (e) => e.target,
        currentTarget: (e) => e.currentTarget,
        keyboardKey: (e) => e.key,
        keyboardAltKey: (e) => e.altKey,
        keyboardCtrlKey: (e) => e.ctrlKey,
        inputType: (e) => e.inputType,
        inputData: (e) => e.data,
        inputTargetRanges: (e) => e.getTargetRanges()
      },
      range: {
        startContainer: (r) => r.startContainer,
        startOffset: (r) => r.startOffset,
        endContainer: (r) => r.endContainer,
        endOffset: (r) => r.endOffset
      },
      selection: {
        anchorNode: (s) => s.anchorNode,
        anchorOffset: (s) => s.anchorOffset,
        focusNode: (s) => s.focusNode,
        focusOffset: (s) => s.focusOffset,
        isCollapsed: (s) => s.isCollapsed,
        collapse: (s, node, offset) => s.collapse(node, offset),
        setBaseAndExtent: (s, an, ao, fn, fo) => s.setBaseAndExtent(an, ao, fn, fo),
        setPosition: (s, node, offset) => s.setPosition(node, offset)
      }
    }
  });
}

window.addEventListener("load", load);
