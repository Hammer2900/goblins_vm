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
             (gnu packages ninja)
             (gnu packages python)
             (gnu packages tor)
             (guix)
             (guix build-system gnu)
             (guix download)
             (guix gexp)
             (guix git)
             (guix git-download)
             ((guix licenses)
              #:prefix license:)
             (guix packages)
             (ice-9 match)
             (srfi srfi-1))

(define %here (dirname (current-filename)))

(define guile-hoot*
  (let ((commit "f47f53dd7b6e56c10f1a034e22d85cf092867327"))
    (package
      (inherit guile-hoot)
      (version (string-append (package-version guile-hoot)
                              "-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/spritely/guile-hoot.git")
                      (commit commit)))
                (sha256
                 (base32
                  "0d6bsl3yygsfjrk9mmxkyq9lfcci3z6kk58cs2blqn8j13ydz51s"))))
      (arguments
       '(#:tests? #f)))))

(define node
  (package
    (name "node")
    (version "23.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.xz"))
              (sha256
               (base32 "0w9kgvx66fs5rrbv2qc7wx8zq1j0pzbhaysk589bp47pzbrlpcab"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
             (let ((prefix (assoc-ref outputs "out")))
               (setenv "CC_host" "gcc")
               (setenv "CXX_host" "g++")
               (setenv "CC" ,(cc-for-target))
               (setenv "CXX" ,(cxx-for-target))
               (invoke (search-input-file (or native-inputs inputs)
                                          "/bin/python3")
                       "configure"
                       (string-append "--prefix=" prefix))))))))
    (native-inputs (list ninja python))
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (synopsis "Evented I/O for V8 JavaScript")
    (description
     "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (home-page "https://nodejs.org/")
    (license license:expat)))

(define (replace-guile pkg)
  (package
    (inherit pkg)
    (inputs
     (modify-inputs (package-inputs pkg)
                    (replace "guile" guile-next)))))

(packages->manifest
 (cons*
  graphviz
  guile-hoot*
  (replace-guile guile-lib)
  (replace-guile guile-syntax-highlight)
  lcov
  node
  tor
  (filter-map
   (match-lambda
     ((_ (? package? package) output) (list package output))
     ((_ (? package? package)) package)
     (else #f))
   (package-development-inputs
    (load (string-append %here "/guix.scm"))))))
