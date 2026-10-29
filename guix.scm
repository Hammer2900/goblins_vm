;; This file is waived into the public domain, as-is, no warranty provided.
;;
;; If the public domain doesn't exist where you live, consider
;; this a license which waives all copyright and neighboring intellectual
;; restrictions laws mechanisms, to the fullest extent possible by law,
;; as-is, no warranty provided.
;;
;; No attribution is required and you are free to copy-paste and munge
;; into your own project.

(use-modules
  (guix gexp)
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix git-download)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages code)
  (gnu packages graphviz)
  (gnu packages gnupg)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages tls)
  (gnu packages tor)
  (gnu packages node)
  (gnu packages python)
  (gnu packages python-crypto)
  ((guix licenses) #:prefix license:)
  (srfi srfi-1))

(define (keep-file? file stat)
  (not (any (lambda (my-string)
              (string-contains file my-string))
            (list ".git" ".dir-locals.el" "guix.scm"))))

(define guile-noise
  (package
   (name "guile-noise")
   (version "0.1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://codeberg.org/spritely/guile-noise.git")
                  (commit "542410fd60f454c318f8efae817046d03fa3290d")))
            (sha256
             (base32 "1bwi9kasm146sgwqjgmgclwlzvyan8738xdwmcivkjy380zdnlny"))))
   (build-system gnu-build-system)
   (native-inputs (list autoconf automake pkg-config))
   (inputs (list guile-next guile-hoot
                 ;; Only needed to run test suite (make check)
                 node python python-cryptography))
   (propagated-inputs (list guile-gnutls))
   (synopsis "Guile implementation of the Noise Protocol")
   (description "Guile implementation of the Noise Protocol")
   (home-page "https://codeberg.org/spritely/guile-noise")
   (license license:asl2.0)))

(package
  (name "guile-goblins")
  (version "0.18.0-git")
  (source (local-file (dirname (current-filename))
                      #:recursive? #t
                      #:select? keep-file?))
  (build-system gnu-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (replace 'bootstrap
         (lambda _
           (invoke "autoreconf" "-vif"))))
     #:make-flags
     ,#~(list "GUILE_AUTO_COMPILE=0")))
  (native-inputs
   (list autoconf automake pkg-config texinfo
         ;; Technically not needed for building or running goblins but they're
         ;; useful to have around for development.
         guile-lib guile-syntax-highlight lcov tor))
  (inputs (list graphviz guile-next guile-hoot))
  (propagated-inputs
   (list guile-fibers guile-gnutls guile-noise guile-websocket guile-bstructs))
  (synopsis "Transactional, distributed object programming environment")
  (description
   "Spritely Goblins is a transactional, distributed object programming
environment following object capability principles.  This is the guile
version of the library!")
  (home-page "https://spritely.institute/goblins")
  (license license:asl2.0))
