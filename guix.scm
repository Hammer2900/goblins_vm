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
  (gnu packages gnupg)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages tls)
  ((guix licenses) #:prefix license:)
  (srfi srfi-1))

(define (keep-file? file stat)
  (not (any (lambda (my-string)
              (string-contains file my-string))
            (list ".git" ".dir-locals.el" "guix.scm"))))

(define guile-hoot*
  (let ((commit "54b0b68fa7ec246d538e27384d8f6cd02ce89f80"))
    (package
      (inherit guile-hoot)
      (version (string-append (package-version guile-hoot)
                              "-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://codeberg.org/spritely/hoot.git")
                       (commit commit)))
                (sha256
                 (base32
                  "003ha3nq5sfb346ky61v2zxkca8w7189z8fn0g76l9xw71vs5pw4"))))
      (arguments '(#:tests? #f))
      (native-search-paths
       (list (search-path-specification
               (variable "HOOT_LOAD_PATH")
               (files (list "share/guile-hoot/site"))))))))

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
   (list autoconf automake pkg-config texinfo))
  (inputs (list guile-next guile-hoot*))
  (propagated-inputs
   (list guile-fibers guile-gnutls guile-websocket guile-bstructs))
  (synopsis "Transactional, distributed object programming environment")
  (description
   "Spritely Goblins is a transactional, distributed object programming
environment following object capability principles.  This is the guile
version of the library!")
  (home-page "https://spritely.institute/goblins")
  (license license:asl2.0))
