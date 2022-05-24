;;; Copyright 2021 Christine Lemmer-Webber
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

;; TODO: Kinda misnamed just because copied straight from the racket code
(define-module (goblins ocapn structs-urls)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  :use-module  (srfi srfi-9)
  #:use-module (goblins ocapn define-recordable)
  #:export (<ocapn-machine>
	    ocapn-machine
            make-ocapn-machine
            ocapn-machine?
            ocapn-machine-transport
            ocapn-machine-address
            ocapn-machine-hints
	    ocapn-struct?
	    ocapn-struct->ocapn-machine
	    uri->ocapn-machine
	    uri->ocapn-sturdyref
	    uri->ocapn-cert
	    uri->ocapn-bearer-union
	    same-machine-location?
            marshall::ocapn-machine
            unmarshall::ocapn-machine

	    <ocapn-sturdyref>
            ocapn-sturdyref
            ocapn-sturdyref?
            make-ocapn-sturdyref
            ocapn-sturdyref-machine
            ocapn-sturdyref-swiss-num
            marshall::ocapn-sturdyref
            unmarshall::ocapn-sturdyref

	    <ocapn-cert>
            ocapn-cert
            ocapn-cert?
            make-ocapn-cert
            ocapn-cert-machine
            ocapn-cert-certdata
            marshall::ocapn-cert
            unmarshall::ocapn-cert

	    <ocapn-bearer-union>
            ocapn-bearer-union
            ocapn-bearer-union?
            ocapn-bearer-union-cert
            ocapn-bearer-union-key-type
            ocapn-bearer-union-private-key
            marshall::ocapn-bearer-union
            unmarshall::ocapn-bearer-union))


;; Ocapn machine type URI:
;;
;;   ocapn:m.<transport>.<transport-address>[.<transport-hints>]
;;
;;   <ocapn-machine $transport $transport-address $transport-hints>
;;
;; . o O (Are hints really a good idea or needed anymore?)

;; EG: "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
(define-recordable ocapn-machine
  (transport address hints))

;; Ocapn swissnum URI:
;;
;;   ocapn:s.onion.abpoiyaspodyoiapsdyiopbasyop/3cbe8e02-ca27-4699-b2dd-3e284c71fa96
;;
;;   ocapn:s.<transport>.<transport-address>/<swiss-num>
;;
;;   <ocapn-sturdyref <ocapn-machine $transport $transport-address $transport-hints>
;;                    $swiss-num>

(define-recordable ocapn-sturdyref
  (machine swiss-num))

;; Ocapn certificate URI:
;;
;;   ocapn:c.<transport>.<transport-address>/<cert>
;;
;;   <ocapn-cert <ocapn-machine $transport $transport-address $transport-hints>
;;               $cert>

(define-recordable ocapn-cert
  (machine certdata))

;; Ocapn bearer certificate union URI:
;;
;;   ocapn:b.<transport>.<transport-address>/<cert>/<key-type>.<private-key>
;;
;;   <ocapn-bearer-union <ocapn-cert <ocapn-machine $transport
;;                                                  $transport-address
;;                                                  $transport-hints>
;;                                   $cert>
;;                       $key-type
;;                       $private-key>

(define-recordable ocapn-bearer-union
  (cert key-type private-key))


;; Some of these are commented out as they're not supported, however
;; we want to support them in future.
(define (ocapn-struct? obj)
  (or (ocapn-machine? obj)
      (ocapn-sturdyref? obj)
      (ocapn-bearer-union? obj)
      (ocapn-cert? obj)))

(define (ocapn-struct->ocapn-machine ocapn-struct)
  (match ocapn-struct
    [(? ocapn-machine?) ocapn-struct]
    [($ <ocapn-sturdyref> ocapn-machine _sn) ocapn-machine]
    [($ <ocapn-cert> ocapn-machine _cert) ocapn-machine]
    [($ <ocapn-bearer-union> ($ <ocapn-cert> ocapn-machine _cert) _key-type _private-key)
     ocapn-machine]))

;; ;; Checks for the equivalence between two ocapn-machine structs
;; ;; (including ocapn-machines nested in other ocapn-structs),
;; ;; ignoring hints
(define (same-machine-location? ocapn-struct1 ocapn-struct2)
  (define machine1 (ocapn-struct->ocapn-machine ocapn-struct1))
  (define machine2 (ocapn-struct->ocapn-machine ocapn-struct2))
  (match-let ((($ <ocapn-machine> m1-transport m1-address _m1-hints)
               machine1)
              (($ <ocapn-machine> m2-transport m2-address _m2-hints)
               machine2))
    (and (equal? m1-transport m2-transport)
         (equal? m1-address m2-address))))


(define-record-type <ocapn-uri>
  (make-ocapn-uri type transport address path)
  ocapn-uri?
  (type ocapn-uri-type)
  (transport ocapn-uri-transport)
  (address ocapn-uri-address)
  (path ocapn-uri-path))

(define string->ocapn-uri
  (let* ((type-pat "[A-z0-9]+")
	 (transport-pat "[A-z0-9]+")
	 (address-pat "[A-z0-9\\.]+")
	 (path-pat ".*")
	 (uri-pat
	  (format #f "ocapn:(~a)\\.(~a)\\.(~a)[\\/]?(~a)"
		  type-pat transport-pat address-pat path-pat))
	 (uri-regex (make-regexp uri-pat)))
    (lambda (uri)
      (let ((m (regexp-exec uri-regex uri)))
	(make-ocapn-uri
	 (string->symbol (match:substring m 1))
	 (string->symbol (match:substring m 2))
	 (match:substring m 3)
	 (match:substring m 4))))))

(define (ocapn-uri->string uri)
  (define machine-address
    (string-append
     "ocapn:"
     (symbol->string (ocapn-uri-type uri))
     "."
     (ocapn-uri-transport uri)
     "."
     (ocapn-uri-address uri)))
  (if (eq? (ocapn-uri-type uri) 'm)
      machine-address
      (string-append "/" (ocapn-uri-path uri))))


(define (uri->ocapn-machine machine-address)
  (define uri
    (match machine-address
      ((? string?) (string->ocapn-uri machine-address))
      ((? ocapn-uri?) machine-address)
      (_ (error "Unsupported type"))))
  (ocapn-machine
   (ocapn-uri-transport uri)
   (ocapn-uri-address uri)
   #f))

(define (uri->ocapn-sturdyref studyref)
  (let ((uri (string->ocapn-uri studyref)))
    (when (string-null? (ocapn-uri-path uri))
      (error (format #f "No swiss-num given for uri (~a)"
		     (ocapn-uri->string uri))))
    (ocapn-sturdyref
     (uri->ocapn-machine uri)
     (ocapn-uri-path uri))))

(define (uri->ocapn-cert cert)
  (let ((uri (string->ocapn-uri cert)))
    (when (string-null? (ocapn-uri-path uri))
      (error (format #f "No cert given for uri (~a)"
		     (ocapn-uri->string uri))))
    (ocapn-cert
     (uri->ocapn-machine uri)
     (ocapn-uri-path uri))))

(define (uri->ocapn-bearer-union bearer-union)
  (let ((uri (string->ocapn-uri bearer-union)))
    (define union-parts
      (string-split (ocapn-uri-path uri) #\/))
    (define cert
      (car union-parts))
    (pk 'union-parts union-parts)
    (define-values (key-type private-key)
      (let ((parts (string-split (list-ref union-parts 1) #\.)))
	(values (list-ref parts 0)
		(list-ref parts 1))))

    (ocapn-bearer-union
     (ocapn-cert (uri->ocapn-machine uri) cert)
     (string->symbol key-type)
     private-key)))

;; (define (string->ocapn-machine str)
;;   (url->ocapn-machine (string->url str)))
;; (define (string->ocapn-sturdyref str)
;;   (url->ocapn-sturdyref (string->url str)))
;; (define (string->ocapn-cert str)
;;   (url->ocapn-cert (string->url str)))
;; (define (string->ocapn-bearer-union str)
;;   (url->ocapn-bearer-union (string->url str)))
;; (define (string->ocapn-struct str)
;;   (url->ocapn-struct (string->url str)))

;; (define (ocapn-machine->string machine)
;;   (url->string (ocapn-machine->url machine)))
;; (define (ocapn-sturdyref->string sturdyref)
;;   (url->string (ocapn-sturdyref->url sturdyref)))
;; (define (ocapn-cert->string cert)
;;   (url->string (ocapn-cert->url cert)))
;; (define (ocapn-bearer-union->string bearer-union)
;;   (url->string (ocapn-bearer-union->url bearer-union)))

;; ;; helper procedure for the next two
;; (define (ocapn-machine->address-str machine)
;;   (match machine
;;     [(ocapn-machine transport address hints)
;;      (if hints
;;          (format "~a.~a.~a" transport address hints)
;;          (format "~a.~a" transport address))]))

;; (define (ocapn-machine->url machine)
;;   (define address-str (ocapn-machine->address-str machine))
;;   (url "ocapn" #f #f #f #f
;;        (list (path/param (string-append "m." address-str) '()))
;;        '() #f))

;; (define (ocapn-sturdyref->url sturdyref)
;;   (match sturdyref
;;     [(ocapn-sturdyref machine swiss-num)
;;      (define address-str (ocapn-machine->address-str machine))
;;      (url "ocapn" #f #f #f #f
;;           (list (path/param (string-append "s." address-str) '())
;;                 (path/param (url-base64-encode swiss-num) '()))
;;           '() #f)]))

;; (define (ocapn-cert->url cert)
;;   (error "TODO: implement ocapn-cert->url"))

;; (define (ocapn-bearer-union->url bearer-union)
;;   (error "TODO: implement ocapn-bearer-union->url"))

;; (define (ocapn-url? x)
;;   (and (url? x)
;;        (equal? (url-scheme x) "ocapn")))

;; (define-syntax-rule (define-ocapn-url-checker id prefix)
;;   (define (id x)
;;     (match x
;;       [(url "ocapn" #f #f #f #f (cons (path/param type-address '())
;;                                       _other-paths)
;;             '() #f)
;;        (string-prefix? type-address prefix)]
;;       [_ #f])))

;; (define-ocapn-url-checker ocapn-machine-url? "m.")
;; (define-ocapn-url-checker ocapn-sturdyref-url? "s.")
;; (define-ocapn-url-checker ocapn-cert-url? "c.")
;; (define-ocapn-url-checker ocapn-bearer-union-url? "u.")



;; (module+ test
;;   (require rackunit)

;;   (test-equal?
;;    "url->ocapn-machine, no hints"
;;    (url->ocapn-machine
;;     (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"))
;;    (ocapn-machine 'onion
;;                   "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                   #f))

;;   (test-equal?
;;    "url->ocapn-machine, with hints"
;;    (string->ocapn-machine
;;     "ocapn:m.foo.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.blablabla")
;;    (ocapn-machine 'foo
;;                   "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                   "blablabla"))

;;   (test-equal?
;;    "url->ocapn-sturdyref"
;;    (url->ocapn-sturdyref (string->url "ocapn:s.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/eQjP4nR28ffX5eoNCK5DWH6DT_d7BIqD3-My459CUbU"))
;;    (ocapn-sturdyref
;;     (ocapn-machine 'onion
;;                    "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                    #f)
;;     #"y\b\317\342tv\361\367\327\345\352\r\b\256CX~\203O\367{\4\212\203\337\3432\343\237BQ\265"))

;;   (test-equal?
;;    "ocapn-machine->url, no hints"
;;    (ocapn-machine->url
;;     (ocapn-machine 'onion
;;                    "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                    #f))
;;    (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"))

;;   (test-equal?
;;    "ocapn-machine->url, with hints"
;;    (ocapn-machine->url
;;     (ocapn-machine 'onion
;;                    "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                    "hintyhint"))
;;    (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd.hintyhint"))

;;   (test-equal?
;;    "ocapn-sturdyref->url"
;;    (ocapn-sturdyref->url
;;     (ocapn-sturdyref
;;      (ocapn-machine 'onion
;;                     "wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd"
;;                     #f)
;;      #"y\b\317\342tv\361\367\327\345\352\r\b\256CX~\203O\367{\4\212\203\337\3432\343\237BQ\265"))
;;    (string->url "ocapn:s.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/eQjP4nR28ffX5eoNCK5DWH6DT_d7BIqD3-My459CUbU"))

;;   (check-true
;;    (ocapn-machine-url? (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd")))
;;   (check-true
;;    (ocapn-sturdyref-url? (string->url "ocapn:s.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/eQjP4nR28ffX5eoNCK5DWH6DT_d7BIqD3-My459CUbU")))
;;   (check-true
;;    (ocapn-cert-url? (string->url "ocapn:c.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/PDknc29tZS1jZXJ0NCd3aXRoNCdzb21lNCdkYXRhPg")))
;;   (check-true
;;    (ocapn-bearer-union-url? (string->url "ocapn:u.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd/PDknc29tZS1jZXJ0NCd3aXRoNCdzb21lNCdkYXRhPg/fake.IPu0dfs0j-uBRbNgF0gc6zTH2UGujJ-SLyAlhAABSW0")))
;;   (check-false
;;    (ocapn-sturdyref-url? (string->url "ocapn:m.onion.wy46gxdweyqn5m7ntzwlxinhdia2jjanlsh37gxklwhfec7yxqr4k3qd")))

;;   (check-true
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-machine "ocapn:m.testnet.foo.blahblah")))
;;   (check-false
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-machine "ocapn:m.testnet.bar")))
;;   (check-false
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-machine "ocapn:m.othernet.foo")))
;;   (check-true
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-sturdyref "ocapn:s.testnet.foo/d1607782-3c39-463f-baae-408753681a91")))
;;   (check-false
;;    (same-machine-location?
;;     (string->ocapn-machine "ocapn:m.testnet.foo")
;;     (string->ocapn-sturdyref "ocapn:s.testnet.bar/d1607782-3c39-463f-baae-408753681a91"))))
;; )
