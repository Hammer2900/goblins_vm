;; This file is waived into the public domain, as-is, no warranty provided.
;;
;; If the public domain doesn't exist where you live, consider
;; this a license which waives all copyright and neighboring intellectual
;; restrictions laws mechanisms, to the fullest extent possible by law,
;; as-is, no warranty provided.
;;
;; No attribution is required and you are free to copy-paste and munge
;; into your own project.

(use-modules (gnu packages code)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages graphviz)
             (gnu packages guile-xyz)
             (gnu packages tor)
             (guix git-download)
             (guix packages)
             (ice-9 match)
             (srfi srfi-1))

(define %here (dirname (current-filename)))

(define guile-websocket*
  (let ((commit "795dd67dee3dc3f5fa791a20d4b51cc4e811b5d6"))
    (package
     (inherit guile-websocket)
     (version (string-append (package-version guile-websocket)
                             "-1." (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.dthompson.us/guile-websocket.git")
                    (commit commit)))
              (sha256
               (base32
                "0q7rkjr2vhry7rn0y1fdd9ir5qkv7y84vc3r5zy5vh5y6irx73gw")))))))

(packages->manifest
 (cons*
  graphviz
  guile-fibers
  guile-lib
  guile-syntax-highlight
  guile-websocket*
  lcov
  tor
  (filter-map
   (match-lambda
     ((_ (? package? package) output) (list package output))
     ((_ (? package? package)) package)
     (else #f))
   (package-development-inputs
    (load (string-append %here "/guix.scm"))))))
