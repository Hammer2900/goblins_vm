;;; Copyright 2023-2026 Jessica Tallon
;;; Copyright 2020-2022 Christine Lemmer Webber
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

(define-module (goblins cli relay-server)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib ward)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins utils ghash)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer onion)
  #:use-module (goblins ocapn netlayer websocket)
  #:use-module (goblins ocapn netlayer relay)
  #:use-module (goblins ocapn netlayer relay-utils)
  #:use-module (goblins persistence-store bloblin)
  #:use-module (fibers conditions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:export (relay-server-command))


(define (ocapn-sturdyref->netlayer-name sref)
  (define peer (ocapn-sturdyref-peer sref))
  (ocapn-peer-transport peer))

(define (spawn-netlayer-by-name name options)
  (match name
    ((or "onion" 'onion)
     (apply spawn ^onion-netlayer (assoc-ref options 'onion-netlayer-args)))
    ((or "websocket" 'websocket)
     (let ((args `(#:host ,(assoc-ref options 'websocket-host)
                   #:port ,(assoc-ref options 'websocket-port)
                   #:encrypted? ,(assoc-ref options 'websocket-encrypted?)
                   #:verify-certificates?
                   ,(assoc-ref options 'websocket-verify-vertificates?)
                   ,@(if (assoc-ref options 'websocket-url)
                         (list #:url (assoc-ref options 'websocket-url))
                         '()))))
       (apply spawn ^websocket-netlayer args)))))

(define (print-help port)
  (format #t "Usage: goblins relay [OPTION...] COMMAND ARGS...

Available options:
-h, --help                       print this help message and exit
--relay-server-store=DIR         Set directory for the persistent store (only
                                 used for the `new-relay' command).

Available websocket options:
--websocket-host=HOST            Set hostname to bind to (default: localhost)
--websocket-port=PORT            Set port to bind to (default: any available)
--websocket-max-peers=MAX-PEERS  Set maximum peers who can be simultaneously
                                 connected at once (default: 32).
--websocket-no-encryption        Disables tls/encryption
--websocket-no-verify            Disables verification of TLS certificates.
--websocket-url=URL              Set custom URL (default: ws(s)://HOSt:PORT)

Available tor onion options:
--tor-control-path=DIR           Set the directory for tor daemon control path
                                 (default: ~~/.cache/goblins/tor/tor-control-sock).
--tor-socks-path=DIR             Set the directory for the tor daemon socks path
                                 (default: ~~/.cache/goblins/tor/tor-socks-sock)
--tor-ocapn-socks-dir=DIR        Set the directory for the ocapn socks sockets
                                 (default: ~~/.cache/goblins/tor/ocapn-sockets)


Commands:

new-relay CLIENT-NETLAYER FEDERATION-NETLAYER  Creates a new relay server.

list-accounts RELAY-ADMIN-SREF                 List the registered accounts

new-account RELAY-ADMIN-SREF ACCOUNT-LABEL     Add account with given label.

remove-account RELAY-ADMIN-SREF ACCOUNT-LABEL  Remove an account with label
"))

(define (fail message)
  (format (current-error-port) "error: ~a\n" message)
  (exit 1))

(define %main-options
  (list (option '(#\h "help") #f #f
                (lambda (opt name arg result)
                  (print-help (current-output-port))
                  (exit 0)))
        (option '("relay-server-store") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'enable-persistence? #t
                              (alist-cons 'bloblin-store-dir arg result))))))

(define %main-defaults
  '((enable-persistence? . #f)
    (arguments)))

(define %websocket-options
  (list (option '("websocket-host") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'websocket-host arg result)))
        (option '("websocket-port") #t #f
                (lambda (opt name arg result)
                  (define port (string->number arg))
                  (unless (and (exact-integer? port) (<= 0 port))
                    (fail "Bad websocket-port `~a'"))
                  (alist-cons 'websocket-port port result)))
        (option '("websocket-max-peers") #t #f
                (lambda (opt name arg result)
                  (define max-peers (string->number arg))
                  (unless (and (exact-integer? max-peers) (<= 0 max-peers))
                    (fail "Bad websocket-max-peers `~a'"))
                  (alist-cons 'websocket-max-peers max-peers result)))
        (option '("websocket-no-verify") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'websocket-verify-certificates? #f result)))
        (option '("websocket-no-encryption") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'websocket-encryotion? #f result)))
        (option '("websocket-url") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'websocket-url arg result)))))

(define %websocket-defaults
  '((websocket-host . "127.0.0.1")
    (websocket-port . 0)
    (websocket-max-peers . 32)
    (websocket-verify-certificates? . #t)
    (websocket-encryption? . #t)
    (websocket-url . #f)))

(define %onion-options
  (list (option '("tor-control-path") #t #f
                (lambda (opt name arg result)
                  (let ((args (assoc-ref result 'onion-netlayer-args)))
                    (alist-cons 'onion-netlayer-args
                                (cons* #:tor-control-path arg args)
                                result))))
        (option '("tor-socks-path") #t #f
                (lambda (opt name arg result)
                  (let ((args (assoc-ref result 'onion-netlayer-args)))
                    (alist-cons 'onion-netlayer-args
                                (cons* #:tor-socks-path arg args)
                                result))))
        (option '("tor-ocapn-socks-dir") #t #f
                (lambda (opt name arg result)
                  (let ((args (assoc-ref result 'onion-netlayer-args)))
                    (alist-cons 'onion-netlayer-args
                                (cons* #:tor-ocapn-socks-dir arg args)
                                result))))))

(define %onion-defaults
  '((onion-netlayer-args)))

(define %options (append %main-options %websocket-options %onion-options))
(define %defaults (append %main-defaults %websocket-defaults %onion-defaults))

(define (parse-arguments args)
  (args-fold args %options
             (lambda (opt name arg result)
               (fail (format #f "unrecognized option: ~a" name)))
             (lambda (arg result)
               (let ((arguments (assoc-ref result 'arguments)))
                 (alist-cons 'arguments (cons arg arguments) result)))
             %defaults))

(define server-persistence-env
  (persistence-env-compose relay-utils-env
                           websocket-netlayer-env
                           onion-netlayer-env))

(define (relay-server-command command-line-arguments)
  (define options (parse-arguments command-line-arguments))
  (define can-quit? (make-condition))
  (match (reverse (assoc-ref options 'arguments))
    (("new-relay" client-netlayer-str federation-netlayer-str)
     (define client-netlayer-name (string->symbol client-netlayer-str))
     (define federation-netlayer-name (string->symbol federation-netlayer-str))
     (define (spawn-relay-server)
       (define client-netlayer
         (spawn-netlayer-by-name client-netlayer-name options))
       (define mycapn (spawn-mycapn client-netlayer))
       (unless (eq? client-netlayer-name federation-netlayer-name)
         (let ((federation-netlayer
                (spawn-netlayer-by-name federation-netlayer-name options)))
           ($ mycapn 'install-netlayer federation-netlayer)))
       (define-values (warden incanter)
         (spawn-warding-pair))
       (define relay-admin
         (spawn ^relay-admin warden mycapn client-netlayer-name
                federation-netlayer-name))
       (values mycapn relay-admin (enchant incanter relay-admin)))
     (define-values (vat mycapn relay-admin enchanted-ra)
       (cond ((assoc-ref options 'enable-persistence?)
              (spawn-persistent-vat
               server-persistence-env
               spawn-relay-server
               (make-bloblin-store (assoc-ref options 'bloblin-store-dir))))
              (else
               (let*-values (((vat) (spawn-vat #:name "Relay server"))
                             ((mycapn relay-admin enchanted-relay-admin)
                              (call-with-vat vat spawn-relay-server)))
                 (values vat mycapn relay-admin enchanted-relay-admin)))))
     (with-vat vat
       (let-on ((public-ra-sref
                 (<- mycapn 'register relay-admin client-netlayer-name))
                (enchanted-ra-sref
                 (<- mycapn 'register enchanted-ra client-netlayer-name)))
         (format #t "Public Relay: ~a\n" (ocapn-id->string public-ra-sref))
         (format #t "Admin Relay: ~a\n" (ocapn-id->string enchanted-ra-sref)))))
      (("register-account" relay-admin account-name)
       (define relay-admin-sref (string->ocapn-id relay-admin))
       (define vat (spawn-vat #:name "Relay server command"))
       (with-vat vat
         (define netlayer-name
           (ocapn-sturdyref->netlayer-name relay-admin-sref))
         (define netlayer
           (spawn-netlayer-by-name netlayer-name options))
         (define mycapn (spawn-mycapn netlayer))
         (define relay-admin-vow (<- mycapn 'enliven relay-admin-sref))
         (let-on ((account (<- relay-admin-vow 'register-account account-name)))
           (format #t "Account: ~a\n" (ocapn-id->string account))
           (signal-condition! can-quit?))))
    (("remove-account" relay-admin account-name)
     (define relay-admin-sref (string->ocapn-id relay-admin))
     (define vat (spawn-vat #:name "Relay server command"))
     (with-vat vat
       (define netlayer-name (ocapn-sturdyref->netlayer-name relay-admin-sref))
       (define netlayer (spawn-netlayer-by-name netlayer-name options))
       (define mycapn (spawn-mycapn netlayer))
       (define relay-admin-vow (<- mycapn 'enliven relay-admin-sref))
       (let-on ((_ (<- relay-admin-vow 'remove-account account-name)))
         (format #t "Account ~a has been removed.\n" account-name)
         (signal-condition! can-quit?))))
    (("list-accounts" relay-admin)
     (define relay-admin-sref (string->ocapn-id relay-admin))
     (define vat (spawn-vat #:name "Relay server command"))
     (with-vat vat
       (define netlayer-name (ocapn-sturdyref->netlayer-name relay-admin-sref))
       (define netlayer (spawn-netlayer-by-name netlayer-name options))
       (define mycapn (spawn-mycapn netlayer))
       (define relay-admin-vow (<- mycapn 'enliven relay-admin-sref))
       (let-on ((accounts (<- relay-admin-vow 'get-accounts)))
         (format #t "Accounts:\n")
         (hashmap-for-each
          (match-lambda*
           ((label (type _ _ _ expiry))
            (if expiry
                (format #t "- ~a (type: ~a) expires on ~a\n"
                        label type (strftime "%c" (localtime expiry)))
                (format #t "- ~a (type: ~a)\n" label type))))
          accounts)
         (signal-condition! can-quit?))))
    (()
     (print-help (current-output-port))
     (signal-condition! can-quit?))
    (option
     ;; Unknown just display the help.
     (fail (format #f "unknown option: ~a" option))))
  (wait can-quit?))
