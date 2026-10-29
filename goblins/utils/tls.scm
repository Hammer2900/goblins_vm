;;; Copyright 2023 David Thompson
;;; Copyright 2024-2026 Jessica Tallon
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
(define-library (goblins utils tls)
  (export generate-tls-private-key
          generate-tls-certificate
          load-tls-private-key
          load-tls-certificate)
  (import (guile)
          (rnrs bytevectors)
          (ice-9 binary-ports))
  (cond-expand
   (guile (import (gnutls)))
   (hoot))
  (begin
    (cond-expand
     (guile
      ;; The GnuTLS bindings used for generating keys/certs are only in
      ;; bleeding edge versions of guile-gnutls, so we cannot rely on them
      ;; being there.
      (define (gnutls-procedure-exists? name)
        (variable? (module-variable (resolve-interface '(gnutls)) name)))

      (define (%generate-tls-private-key)
        ;; For compatibility with older Guile-GnuTLS versions that do not
        ;; support generation and exporting, we have to hand back a
        ;; bytevector that can be imported into a TLS session.
        (let ((key (generate-x509-private-key pk-algorithm/rsa 4096 '())))
          (export-x509-private-key key x509-certificate-format/pem)))

      (define (generate-tls-private-key)
        "Return a freshly generated X.509 private key."
        (if (gnutls-procedure-exists? 'generate-x509-private-key)
            (%generate-tls-private-key)
            (throw 'missing-gnutls-binding 'generate-x509-private-key)))

      (define (%generate-tls-certificate key)
        (let* ((cert (make-x509-certificate))
               ;; We're stuck with bytevectors for holding key data so that
               ;; we stay compatible with older versions of Guile-GnuTLS,
               ;; hence this silly re-import here.
               (key* (import-x509-private-key key x509-certificate-format/pem))
               (activation-time (current-time))
               ;; The maximum possible expiration time, at least the maximum
               ;; allowed by GnuTLS, is the end of the UNIX epoch in 2038.
               ;; Not great!
               (expiration-time 2147483647))
          (set-x509-certificate-version! cert 3)
          (set-x509-certificate-activation-time! cert activation-time)
          (set-x509-certificate-expiration-time! cert expiration-time)
          ;; A lot of this data is just boilerplate so that all the
          ;; necessary fields are populated for the certificate to be valid.
          (set-x509-certificate-dn-by-oid! cert oid/x520-country-name "US")
          (set-x509-certificate-dn-by-oid! cert oid/x520-state-or-province-name
                                           "DE")
          (set-x509-certificate-dn-by-oid! cert oid/x520-locality-name "Lewes")
          (set-x509-certificate-dn-by-oid! cert oid/x520-common-name "CapTP")
          (set-x509-certificate-dn-by-oid! cert oid/x520-organization-name
                                           "Spritely")
          (set-x509-certificate-dn-by-oid! cert
                                           oid/x520-organizational-unit-name
                                           "Goblins")
          ;; We aren't using CAs.
          (set-x509-certificate-ca-status! cert #f)
          ;; Likewise, the serial number is really only important for CAs.
          ;; The cert has to have one, though, so we hardcode a silly
          ;; message that conforms to the conventional 20 byte length.
          (set-x509-certificate-serial! cert (string->utf8 "OCapN cereal number!"))
          ;; Add the private key and self-sign the cert.
          (set-x509-certificate-key! cert key*)
          (set-x509-certificate-key-usage! cert
                                           (list key-usage/digital-signature
                                                 key-usage/key-encipherment))
          (set-x509-certificate-subject-key-id! cert (x509-certificate-key-id cert))
          (sign-x509-certificate! cert cert key*)
          ;; For compatibility with older Guile-GnuTLS versions that do not
          ;; support generation and exporting, we have to hand back a
          ;; bytevector that can be imported into a TLS session.
          (export-x509-certificate cert x509-certificate-format/pem)))

      (define (generate-tls-certificate key)
        "Return a freshly generated X.509 certificate for KEY."
        (if (gnutls-procedure-exists? 'make-x509-certificate)
            (%generate-tls-certificate key)
            (throw 'missing-gnutls-binding 'make-x509-certificate))))
     (hoot
      (define (generate-tls-private-key)
        (error "Cannot generate TLS private key on Hoot"))
      (define (generate-tls-certificate key)
        (error "Cannot generate TLS certificate on Hoot"))))

    (define (load-bytevector file-name)
      (call-with-input-file file-name get-bytevector-all))

    (define (load-tls-private-key file-name)
      "Load the PEM encoded X.509 private key in FILE-NAME."
      (load-bytevector file-name))

    (define (load-tls-certificate file-name)
      "Load the PEM encoded X.509 certificate in FILE-NAME."
      (load-bytevector file-name))))