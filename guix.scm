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
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages gnupg)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages tls)
  (srfi srfi-1)
  (srfi srfi-98))

;; This is needed for generating x509 certificates, we should remove it and rely
;; on guix's package for this once it's in the repos.
(define guile-gnutls-3.7.14
  (package
    (inherit guile-gnutls)
    (name "guile-gnutls")
    (version "3.7.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gitlab.com/gnutls/guile/uploads/"
                    "1fdc941351d54cd7affda1bb912b9ca5"
                    "/guile-gnutls-" version ".tar.gz"))
              (sha256
               (base32
                "0ldnxq5qxzy92jd8w5c717bgx4038x9qmi43bzl6kmlkzpagqayy"))))))

;; Compiling guile-gnutls seems to fail in CI, probably being killed for using
;; too many resources. It's difficult to debug. Since we don't currently have
;; any tests needing to generate x509 certs, fallback to the pkg from guix in CI.
(define guile-gnutls-pkg
  (if (get-environment-variable "CI")
      guile-gnutls
      guile-gnutls-3.7.14))

(define (keep-file? file stat)
  (not (any (lambda (my-string)
              (string-contains file my-string))
            (list ".git" ".dir-locals.el" "guix.scm"))))

(package
  (name "guile-goblins")
  (version "0.11.0-git")
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
   (list
     autoconf
     automake
     pkg-config
     texinfo))
  (inputs (list guile-3.0))
  (propagated-inputs
   (list guile-fibers guile-gcrypt guile-gnutls-pkg))
  (synopsis "Transactional, distributed object programming environment")
  (description
   "Spritely Goblins is a transactional, distributed object programming
environment following object capability principles.  This is the guile version
of the library!")
  (home-page "https://spritelyproject.org/")
  (license license:asl2.0))
