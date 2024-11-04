;;; Copyright 2024 Juliana Sims
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

(define-module (goblins ocapn netlayer insecure-websocket)
  #:use-module (fibers io-wakeup)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module ((goblins core) #:renamer (lambda (x) (if (eq? x '$) '$C x)))
  #:use-module (goblins define-actor)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins contrib base64)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins ocapn ids)
  #:use-module ((goblins ocapn netlayer utils)
                #:select (use-nonblocking-i/o))
  #:use-module (goblins utils base32)
  #:use-module (goblins utils crypto)
  #:use-module (ice-9 match)
  #:use-module ((scheme base)
                #:select (bytevector-copy))
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web socket frame)
  #:use-module (web socket utils)
  #:export (^insecure-websocket-netlayer
            insecure-websocket-netlayer-env))

(define (^websocket-server-io _bcom host port)
  "Construct an IO actor to wrap a WebSocket server socket"
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (use-nonblocking-i/o sock)
    (^io _bcom sock
         #:init
         (lambda (sock)
           (bind sock AF_INET (inet-pton AF_INET host) port)
           (listen sock 1))
         #:cleanup close-port)))

(define (websocket-server-handshake client-socket)
  "Perform the server-side handshake for a WebSocket connection over
@var{client-socket}"
  (define (make-handshake-response key)
    ;; Server-side
    "Return an HTTP response object for upgrading to a WebSocket
connection for the client whose key is CLIENT-KEY, a base64 encoded
string."
    ;; See section 4.2.2.
    (let ((accept-key (make-accept-key (string-trim-both key))))
      (build-response #:code 101
                      #:headers `((upgrade . ("websocket"))
                                  (connection . (upgrade))
                                  (sec-websocket-accept . ,accept-key)))))
  (with-exception-handler (lambda (exn) (close-port client-socket))
    (lambda ()
      (let* ((client-request (read-request client-socket))
             (client-key (assoc-ref (request-headers client-request)
                                    'sec-websocket-key))
             (response (make-handshake-response client-key)))
        (write-response response client-socket)
        (force-output client-socket)
        client-socket))
    #:unwind? #t))

(define (node->client-socket node)
  "Create a port and perform the client-side handshake of a WebSocket connection
with @var{node}"

  (define (make-handshake-request uri key)
    "Create an HTTP request for initiating a WebSocket connection with
the remote resource described by URI, using a randomly generated nonce
KEY."
    (let ((headers `((host . (,(uri-host uri) . #f))
                     (upgrade . ("WebSocket"))
                     (connection . (upgrade))
                     (sec-websocket-key . ,key)
                     (sec-websocket-version . "13"))))
      (build-request uri #:method 'GET #:headers headers)))

  (define (websocket-uri? uri)
    "Return #t if URI is a valid WebSocket URI."
    (and (or (eq? (uri-scheme uri) 'ws)
             (eq? (uri-scheme uri) 'wss))
         (not (uri-fragment uri))))

  (define (websocket-uri->http-uri uri)
    (build-uri (match (uri-scheme uri)
                 ('wss 'https)
                 ('ws 'http))
               #:userinfo (uri-userinfo uri)
               #:host (uri-host uri)
               #:port (uri-port uri)
               #:path (uri-path uri)
               #:query (uri-query uri)
               #:fragment (uri-fragment uri)
               #:validate? #t))

  (let* ((uri (build-uri 'ws
                         #:host
                         (match (assq-ref (ocapn-node-hints node) 'host)
                           (() #f)
                           ((host) host))
                         #:port
                         (or (string->number
                              (match (assq-ref (ocapn-node-hints node) 'port)
                                (() #f)
                                ((port) port)))
                             80)))
         (sock (if (websocket-uri? uri)
                   (open-socket-for-uri (websocket-uri->http-uri uri) #:verify-certificate? #f)
                   (error "not a valid WebSocket URI" uri node))))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (use-nonblocking-i/o sock)
    (let ((key (base64-encode (bytevector-copy (strong-random-bytes 16) 0 16))))
      (write-request (make-handshake-request uri key) sock)
      (force-output sock)
      (let* ((response (read-response sock))
             (headers (response-headers response))
             (upgrade (assoc-ref headers 'upgrade))
             (connection (assoc-ref headers 'connection))
             (accept (assoc-ref headers 'sec-websocket-accept)))
        ;; Validate the handshake.
        (if (and (= (response-code response) 101)
                 (string-ci=? (car upgrade) "websocket")
                 (equal? connection '(upgrade))
                 (string=? (string-trim-both accept) (make-accept-key key)))
            sock
            (begin
              (close-port sock)
              (error "websocket handshake failed" node)))))))

(define (fibers-read-data-frame port)
  ;; This loop is adapted from guile-websocket's
  ;; read-data-frame procedure.  It has been modified to
  ;; integrate with fibers and ping the client if we
  ;; haven't heard from them in awhile.

  ;; Collect frame fragments
  (let loop ((fragments '()) (type #f) (timeouts 0))
    ;; Only allow so many timeouts before giving up.
    (cond
     ((< timeouts 10)
      (match (perform-operation
              (choice-operation
               (wrap-operation (wait-until-port-readable-operation port)
                               (lambda () 'ready))
               (wrap-operation (sleep-operation 30)
                               (lambda () 'timeout))))
        ('ready
         (match (read-frame port)
           (#f
            (close-port port)
            #f)
           ;; Per section 5.4, control frames may appear interspersed
           ;; along with a fragmented message.
           ((? close-frame? frame)
            ;; Per section 5.5.1, echo the close frame back to
            ;; the client before closing the socket.  The
            ;; client may no longer be listening.
            (with-exception-handler (lambda (exn) #f)
              (lambda ()
                (write-frame (make-close-frame (frame-data frame)) port))
              #:unwind? #t)
            (close-port port)
            frame)
           ((? ping-frame? frame)
            ;; Per section 5.5.3, a pong frame must include the exact
            ;; same data as the ping frame.
            (write-frame (make-pong-frame (frame-data frame)) port)
            (loop fragments type 0))
           ((? pong-frame?)             ; silently ignore pongs
            (loop fragments type 0))
           ((? first-fragment-frame? frame) ; begin accumulating fragments
            (loop (list frame) (frame-type frame) 0))
           ((? final-fragment-frame? frame) ; concatenate all fragments
            (make-frame #t type #f (frame-concatenate (reverse fragments))))
           ((? fragment-frame? frame)   ; add a fragment
            (loop (cons frame fragments) type 0))
           ((? data-frame? frame)       ; unfragmented data frame
            frame)
           (frame (error "unexpected WebSocket frame" frame))))
        ;; Ping client if we haven't heard from them in awhile.
        ('timeout
         (write-frame (make-ping-frame #vu8()) port)
         (loop fragments type (1+ timeouts)))))
     (else
      (close-port port)
      #f))))

(define* (^websocket-captp-io _bcom sock #:optional (close-handler values))
  "Construct an actor wrapping a WebSocket connection in a Goblins-friendly way

Type: Actor WebSocket WebSocket -> Actor"
  (define message-io (spawn ^read-write-io sock #:cleanup close-port))

  (methods
   ((read-message unmarshallers)
    (<- message-io 'read
        (lambda (port)
          (match (fibers-read-data-frame port)
            ((or #f (? close-frame?))
             (<-np message-io 'halt)
             (close-handler))
            (message
             (syrup-decode (frame-data message)
                           #:unmarshallers unmarshallers))))))
   ((write-message msg marshallers)
    (<-np message-io 'write
          (lambda (resource)
            (write-frame
             (make-binary-frame
              (syrup-encode msg #:marshallers marshallers))
             resource))))))

(define-actor (^insecure-websocket-netlayer _bcom #:key
                                            (host "127.0.0.1")
                                            (port 8888)
                                            (secure? #f))
  "Unencrypted WebSocket netlayer actor"
  (define our-location
    (make-ocapn-node 'websocket
                     ;; XXX: url-base64-encode uses _ which is not valid for
                     ;; hostnames.  if this errors, just try again
                     ;; TODO: use base32 (not in Hoot atm)
                     (base32-encode (strong-random-bytes #f))
                     `((host ,host)
                       (port ,(number->string port))
                       ;; See sections 3, 4.1
                       (resource-name "/")
                       ;;(secure? ,secure?)
                       ;;(protocol syrup)
                       )))

  ;; Make promises so we can asynchronously pass references to the
  ;; connection-establisher
  (define-values (conn-establisher-vow conn-establisher-resolver)
    (spawn-promise-values))

  ;; Setup our WebSocket server and wrap it in an IO actor
  (define incoming-io (spawn ^websocket-server-io host port))

  (define (start-connection conn-establisher)
    (on (<- incoming-io (lambda (sock)
                          (match (accept sock SOCK_NONBLOCK)
                            ((client-socket . _)
                             (websocket-server-handshake client-socket)))))
        (lambda (client-socket)
          (<-np conn-establisher
                (spawn ^websocket-captp-io client-socket)
                #f))
        #:finally
        (lambda ()
          (start-connection conn-establisher))))

  (methods
   ((netlayer-name) 'websocket)
   ((self-location? loc)
    (same-node-location? our-location loc))
   ((our-location) our-location)
   ;; Initialize our netlayer
   ((setup conn-establisher)
    ;; Resolve our connection-establisher vow to the connection-establisher for
    ;; this session
    (<-np conn-establisher-resolver 'fulfill conn-establisher)
    ;; Start listening for new connections
    (start-connection conn-establisher))
   ;; Connect to the remote OCapN node remote-node
   ((connect-to remote-node)
    (match remote-node
      ;; remote-node is a WebSocket node
      (($ <ocapn-node> 'websocket _ _)
       (on (<- (spawn ^io remote-node) node->client-socket)
           (lambda (client-socket)
             (<- conn-establisher-vow
                 (spawn ^websocket-captp-io client-socket)
                 remote-node))
           #:promise? #t))
      ;; remote-node is not a WebSocket node
      (($ <ocapn-node> transport _ _)
       (error "Mismatched netlayer" transport 'websocket))))))

(define insecure-websocket-netlayer-env
  (make-persistence-env
   `((((goblins ocapn netlayer insecure-websocket) ^insecure-websocket-netlayer)
      ,^insecure-websocket-netlayer))))
