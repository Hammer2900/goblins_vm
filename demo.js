async function load() {
  await Scheme.load_main("demo.wasm", {
    user_imports: {
      document: {
        body() { return document.body; },
        getElementById: Document.prototype.getElementById.bind(document),
        createElement: Document.prototype.createElement.bind(document),
        createTextNode: Document.prototype.createTextNode.bind(document)
      },
      element: {
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
        keyboardKey: (e) => e.key,
        keyboardAltKey: (e) => e.altKey,
        keyboardCtrlKey: (e) => e.ctrlKey
      }
    }
  });
}

window.addEventListener("load", load);
