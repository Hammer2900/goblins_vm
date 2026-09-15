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

(define guile-hoot*
  (let ((commit "fb5213ef8d2889e8d06b4d786b94d06920990750")
        (revision "1"))
    (package
      (inherit guile-hoot)
      (version (string-append (package-version guile-hoot)
                              "-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://codeberg.org/spritely/hoot.git")
                       (commit commit)))
                (sha256
                 (base32
                  "1dz33gjwmvfqhybpk44g1wpdz0cxkxbw5x0ka2bl90l3jl9yqfhy"))))
      (arguments
       (append (package-arguments guile-hoot) (list #:tests? #f))))))

(define guile-gnutls*
  (let ((commit "dd4897fff79364692968295a114e8552c4c4fc71"))
    (package
     (inherit gnutls)
     (version (string-append (package-version guile-gnutls)
                             "-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/tsyesika/guile-gnutls.git")
                    (commit commit)))
              (sha256
               (base32
                "0iqji4yf02a3981lhla44ia0na4cj6shf5c3850n47k90hcjscv1"))))
     (arguments
      (list
       #:configure-flags
       ;; Tell the build system that we want Guile bindings installed to the
       ;; output instead of Guiles own module directory.
       #~(list "--disable-static"
              (string-append "--with-guile-site-dir="
                             "$(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)")
              (string-append "--with-guile-site-ccache-dir="
                             "$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache")
              (string-append "--with-guile-extension-dir="
                             "$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions"))))
     (inputs (append (list (list "autoconf" autoconf)
                           (list "automake" automake)
                           (list "libtool" libtool))
                     (package-inputs guile-gnutls))))))

(define guile-noise-protocol
  (package
   (name "guile-noise-protocol")
   (version "0.1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://codeberg.org/spritely/guile-noise-protocol.git")
                  (commit "5aad5c34908c18eff2f1bd203ae56b5d90bd906d")))
            (sha256
             (base32
              "1z4093whcw78nf6aqm0wnv001iqinziaglyrwyak5gn6sgdh4dn2"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
      (replace 'bootstrap
        (lambda _
          (invoke "autoreconf" "-vif"))))
      #:make-flags
      ,#~(list "GUILE_AUTO_COMPILE=0")))
  (native-inputs (list autoconf automake pkg-config))
  (inputs (list guile-next guile-hoot*))
  (propagated-inputs
   (list guile-gnutls* node python python-cryptography))
  (synopsis "Guile implementation of the Noise Protocol")
  (description "Guile implementation of the Noise Protocol")
  (home-page "https://codeberg.org/spritely/guile-noise-protocol")
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
   (list guile-fibers guile-gnutls* guile-noise-protocol guile-websocket
         guile-bstructs))
  (synopsis "Transactional, distributed object programming environment")
  (description
   "Spritely Goblins is a transactional, distributed object programming
environment following object capability principles.  This is the guile
version of the library!")
  (home-page "https://spritely.institute/goblins")
  (license license:asl2.0))
