;;; Copyright 2020-2021 Christine Lemmer-Webber
;;; Copyright 2023, 2025 Juliana Sims
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

(define-module (goblins actor-lib common)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (goblins core)
  #:use-module (goblins define-actor)
  #:use-module (goblins utils ghash)
  #:use-module (goblins utils hashmap)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:export (^seteq
            ^ghash
            ^hashtable
            ^vector
            common-env))

(define (list->vhash lst)
  (fold (lambda (i vh)
          (vhash-consq i #t vh))
        vlist-null
        lst))

(define (vhash->list vh)
  (vhash-fold (lambda (k v lst)
                (cons k lst))
              (list)
              vh))

(define-actor (^seteq* bcom vh)
  ;; Vhashes don't have a readable format, so use ^seteq as a wrapper to aid
  ;; persistence.
  #:portrait (lambda () (vhash->list vh))
  #:restore (lambda (version . args)
              (spawn ^seteq* (list->vhash args)))
  (methods
   [(add val)
    (bcom (^seteq* bcom (vhash-consq val #t vh)))]
   [(remove val)
    (bcom (^seteq* bcom (vhash-delq val vh)))]
   [(member? val)
    (and (vhash-assq val vh) #t)]
   [(as-list)
    (vhash->list vh)]))

(define* (make-vector-of-cells size #:optional fill)
  (let ((v (make-vector size)))
    (do ((i 0 (1+ i)))
        ((>= i size))
      (vector-set! v i (spawn ^cell fill)))
    v))

;; And the rest, eventually...
(define (^seteq bcom . initial)
  "Construct an actor representing a set where identity is compared
with `eq?'.

Methods:
`add val': Add VAL to the set.
`remove val': Remove VAL from the set.
`member? val': Return #t if VAL is in the set, else #f.
`as-list': Return the set as a cons list."
  (spawn ^seteq* (list->vhash initial)))

(define-actor (^ghash bcom #:optional [ht (make-ghash)])
  "Construct an actor providing a transactional interface to
(goblins utils ghash), a hashmap using `eq?' for refrs and `equal?' for
everything else.

Methods:
`ref key [dflt]': Return the value associated with KEY or DFLT if it is not found.
`set key val': Associate KEY with VAL.
`has-key? key': Return #t if KEY is in the hashtable, else #f.
`remove key': Delete KEY and its associated value.
`data': Return the underlying hashtable."
  #:frozen
  (methods
   [ref
    (case-lambda
      [(key)
       (hashmap-ref ht key)]
      [(key dflt)
       (hashmap-ref ht key dflt)])]
   [(set key val)
    (bcom (^ghash bcom (hashmap-set ht key val)))]
   [(has-key? key)
    (issue-deprecation-warning
     "^ghash's 'has-key? is deprecated. Use 'ref method instead.")
    (let ((none (cons 'no 'value)))
      (not (eq? (hashmap-ref ht key none)
                none)))]
   [(remove key)
    (bcom (^ghash bcom (hashmap-remove ht key)))]
   [(data) ht]))

(define-actor (^hashtable* bcom tier size buckets)
  ;; Use a prime number of buckets to encourage good distribution of
  ;; keys with minimal chaining.
  ;;
  ;; Numbers taken from: https://planetmath.org/goodhashtableprimes
  ;;
  ;; 11 is a popular initial size as seen in Java and elsewhere.
  (define %primes
    #(11 29 53 97 193 389 769 1543 3079 6151 12289 24593 49157 98317
         196613 393241 786433 1572869 3145739 6291469 12582917 25165843
         50331653 100663319 201326611 402653189 805306457 1610612741))
  (define max-tier (1- (vector-length %primes)))
  (define num-buckets (vector-length buckets))
  ;; Min/max size thresholds that trigger resizing + rehashing.
  (define min-size
    (if (zero? tier)
        0
        (inexact->exact (round (* num-buckets 0.25)))))
  (define max-size
    (if (= tier max-tier)
        (inf)
        (inexact->exact (round (* num-buckets 0.75)))))
  (define (cvector-ref v i)
    ($ (vector-ref v i)))
  (define (cvector-set! v i x)
    ($ (vector-ref v i) x))
  (define (fresh-buckets n)
    (make-vector-of-cells n '()))
  (define (rehash! old new)
    (let ((n (vector-length new)))
      (do ((i 0 (1+ i)))
          ((= i (vector-length old)))
        (for-each (lambda (link)
                    (let* ((j (hashg (car link) n))
                           (chain (cvector-ref new j)))
                      (cvector-set! new j (cons link chain))))
                  (cvector-ref old i)))))
  (define (maybe-resize proc)
    (let ((n ($ size)))
      (cond
       ((< n min-size) ; tier down
        (let* ((tier (1- tier))
               (new-buckets (fresh-buckets (vector-ref %primes tier))))
          (rehash! buckets new-buckets)
          (proc new-buckets)
          (bcom (^hashtable* bcom tier size new-buckets))))
       ((> n max-size) ; tier up
        (let* ((tier (1+ tier))
               (new-buckets (fresh-buckets (vector-ref %primes tier))))
          (rehash! buckets new-buckets)
          (proc new-buckets)
          (bcom (^hashtable* bcom tier size new-buckets))))
       (else ; no resize necessary
        (proc buckets)))))
  (define (bucket-fold proc init)
    (let bucket-lp ((i 0) (prev init))
      (if (< i num-buckets)
          (let chain-lp ((chain (cvector-ref buckets i)) (prev prev))
            (if (null? chain)
                (bucket-lp (1+ i) prev)
                (let ((link (car chain)))
                  (chain-lp (cdr chain) (proc (car link) (cdr link) prev)))))
          prev)))
  (methods
   ((ref key #:optional default)
    (let lp ((chain (cvector-ref buckets (hashg key num-buckets))))
      (if (null? chain)
          default
          (let ((link (car chain)))
            (if (equalg? key (car link))
                (cdr link)
                (lp (cdr chain)))))))
   ((set key value)
    (maybe-resize
     (lambda (buckets)
       (let ((i (hashg key (vector-length buckets))))
         (cvector-set! buckets i
                       (let lp ((chain (cvector-ref buckets i)))
                         (cond
                          ((null? chain)
                           ;; Add new key/value pair.
                           ($ size (1+ ($ size)))
                           (list (cons key value)))
                          (else
                           (let ((link (car chain)))
                             (if (equalg? key (car link))
                                 ;; Replace value for existing key.
                                 (cons (cons key value) (cdr chain))
                                 (cons link (lp (cdr chain)))))))))))))
   ((remove key)
    (maybe-resize
     (lambda (buckets)
       (let* ((i (hashg key (vector-length buckets)))
              (chain (let lp ((chain (cvector-ref buckets i)))
                       (and (not (null? chain))
                            (let ((link (car chain)))
                              (cond
                               ((equalg? key (car link))
                                ;; Remove key/value pair from chain.
                                ($ size (1- ($ size)))
                                (cdr chain))
                               (else
                                (let ((result (lp (cdr chain))))
                                  (and result (cons link result))))))))))
         (when chain
           (cvector-set! buckets i chain))))))
   ((keys)
    (bucket-fold (lambda (k v keys) (cons k keys)) '()))
   ((values)
    (bucket-fold (lambda (k v vals) (cons v vals)) '()))))

(define (^hashtable bcom)
  "Spawn and return an actor that stores unordered key/value
pairs.  Keys are compared by @code{eq?} for refrs and by @code{equal?}
for everything else.

Methods:

@itemize
@item @code{ref key [default]}

Return the value associated with @var{key}, otherwise return
@var{default} or @code{#f}.

@item @code{set key value}

Associate @var{key} with @var{value}.

@item @code{remove key}

Remove the association with @var{key}, if present.

@item @code{keys}

Return a list of stored keys.

@item @code{values}

Return a list of stored values.

@end itemize"
  (spawn ^hashtable* 0 (spawn ^cell 0) (make-vector-of-cells 11 '())))

(define-actor (^vector* bcom vec)
  ;; Everything within the vec is within a cell so that the entire
  ;; vector isn't written out each time by aurie every time something
  ;; is set.
  #:frozen
  (methods
   [(ref index) ($ (vector-ref vec index))]
   [(set index new-value) ($ (vector-ref vec index) new-value)]
   [(length) (vector-length vec)]
   [(as-list)
    (let lp ((index 0))
      (if (< index (vector-length vec))
          (cons ($ (vector-ref vec index))
                (lp (1+ index)))
          '()))]
   [(resize new-size #:optional fill)
    (let* ((new-vec (make-vector new-size))
           (old-size (vector-length vec))
           (keep (min old-size new-size)))
      (do ((i 0 (1+ i)))
          ((= i keep))
        (vector-set! new-vec i (vector-ref vec i)))
      (do ((i keep (1+ i)))
          ((>= i new-size))
        (vector-set! new-vec i (spawn ^cell fill)))
      (bcom (^vector* bcom new-vec)))]
   [(fill new-fill-value)
    (do ((i 0 (1+ i)))
        ((>= i (vector-length vec)))
      (let ((element (vector-ref vec i)))
        ($ element new-fill-value)))]))

(define* (^vector bcom size #:optional fill)
  "Return a new vector actor of size @var{size}. The vector elements will
have the initial value of @var{fill}."
  (define vec (make-vector size))
  (do ((i 0 (1+ i)))
      ((>= i size))
    (vector-set! vec i (spawn ^cell fill)))
  (spawn ^vector* vec))

(define common-env
  (make-persistence-env
   `((((goblins actor-lib common) ^ghash) ,^ghash)
     (((goblins actor-lib common) ^hashtable) ,^hashtable*)
     (((goblins actor-lib common) ^seteq) ,^seteq*)
     (((goblins actor-lib common) ^vector) ,^vector*))
   #:extends cell-env))
