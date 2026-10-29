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
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-11)
  #:export (relay-server-command))


(define (ocapn-sturdyref->netlayer-name sref)
  (define peer (ocapn-sturdyref-peer sref))
  (ocapn-peer-transport peer))

(define (spawn-netlayer-by-name name options)
  (match name
    ((or "onion" 'onion) (spawn ^onion-netlayer))
    ((or "websocket" 'websocket)
     (spawn ^websocket-netlayer
            #:host (option-ref options 'websocket-host "127.0.0.1")
            #:port (or (and=> (option-ref options 'websocket-port #f)
                              string->number)
                       0)
            #:encrypted?
            (option->boolean (option-ref options 'websocket-encrypted #f) #t)
            #:verify-certificates?
            (option->boolean
             (option-ref options 'websocket-verify-certificates? #f)
             #t)))))

(define (option-boolean? val)
  (match val
    ((or "t" "f" "true" "false") #t)
    (_ #f)))

(define* (option->boolean val #:optional default)
  (match val
    ((or "t" "true") #t)
    ((or "f" "false") #f)
    (_ default)))

(define options-spec
  `((help (single-char #\h) (value #f))
    ;; Websocket netlayer options
    (websocket-host (value #t))
    (websocket-port (value #t))
    (websocket-encrypted (value #t) (predicate ,option-boolean?))
    (websocket-verify-certificates (value #t))))

(define server-persistence-env
  (persistence-env-compose relay-utils-env
                           websocket-netlayer-env
                           onion-netlayer-env))

(define (display-help)
  (format #t "Relay server:

General options:
-h, --help                                                 Displays this help

Main options:
new-relay <client-netlayer> <federation-netlayer>
          [<netlayer options> ...]
register-account <relay-server-sturdyref> <account-name>   Adds a new account with given name.
list-accounts <relay-server-sturdyref>                     Lists all account names configured by this admin.

Currently supported netlayers:
- websocket --websocket-host=<host>                        Defaults: 127.0.0.1
            --websocket-port=<port>                        Defaults: any available port
            --websocket-encrypted=true/false               Defaults: true
            --websocket-verify-certificates=true/false     Defaults: true
- onion\n

To have the server persist itself, specify the path to a directory for the
bloblin store using the RELAY_SERVER_STORE environment variable.\n"))

(define (relay-server-command command-line-arguments)
  (define bloblin-store-path (getenv "RELAY_SERVER_STORE"))

  (define options (getopt-long command-line-arguments options-spec))
  (cond
   ((option-ref options 'help #f) (display-help))
   (else
    (define can-quit? (make-condition))
    ;; Rather unintuitively '() gives you the positional arguments.
    (match (option-ref options '() #f)
      (("relay" "new-relay" client-netlayer-str federation-netlayer-str)
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
         (cond (bloblin-store-path
                (spawn-persistent-vat
                 server-persistence-env
                 spawn-relay-server
                 (make-bloblin-store bloblin-store-path)))
               (else
                (define vat (spawn-vat #:name "Relay server"))
                (define-values (mycapn incanter relay-admin)
                  (call-with-vat vat spawn-relay-server))
                (values vat mycapn relay-admin
                        (enchant incanter relay-admin)))))
       (with-vat vat
         (let-on ((public-ra-sref
                   (<- mycapn 'register relay-admin client-netlayer-name))
                  (enchanted-ra-sref
                   (<- mycapn 'register enchanted-ra client-netlayer-name)))
           (format #t "Public Relay: ~a\n" (ocapn-id->string public-ra-sref))
           (format #t "Admin Relay: ~a\n" (ocapn-id->string enchanted-ra-sref)))))
      (("relay" "register-account" relay-admin account-name)
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
      (("relay" "list-accounts" relay-admin)
       (define relay-admin-sref (string->ocapn-id relay-admin))
       (define vat (spawn-vat #:name "Relay server command"))
       (with-vat vat
         (define netlayer-name
           (ocapn-sturdyref->netlayer-name relay-admin-sref))
         (define netlayer
           (spawn-netlayer-by-name netlayer-name options))
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
      (_
       ;; Unknown just display the help.
       (display-help)
       (signal-condition! can-quit?)))
    (wait can-quit?))))
