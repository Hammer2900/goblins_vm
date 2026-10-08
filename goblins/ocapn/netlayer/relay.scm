;;; Copyright 2023 Christine Lemmer-Webber
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

(define-module (goblins ocapn netlayer relay)
  #:use-module (ice-9 match)
  #:use-module (fibers channels)
  #:use-module (goblins)
  #:use-module (goblins vat)
  #:use-module (goblins inbox)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins actor-lib io)
  #:use-module (goblins actor-lib timers)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn captp)
  #:use-module (goblins ocapn netlayer noise)
  #:use-module (goblins utils base32)
  #:use-module (goblins contrib syrup)
  #:export (relay-sturdyref->relay-peer
            relay-peer->relay-sturdyref
            spawn-relay-pair
            ^relay-netlayer
            relay-env))


;;; URI utils
;;; =========

(define (relay-sturdyref->relay-peer relay-endpoint-sref)
  "Convert RELAY-ENDPOINT-SREF sturdyref into an ocapn peer

This sturdyref represents the underlying relay endpoint."
  (let* ((swiss-num (ocapn-sturdyref-swiss-num relay-endpoint-sref))
         (peer (ocapn-sturdyref-peer relay-endpoint-sref))
         (orig-transport (ocapn-peer-transport peer))
         (orig-designator (ocapn-peer-designator peer))
         (orig-hints (ocapn-peer-hints peer))
         (encoded-designator
          (base32-encode
           (syrup-encode (list orig-designator
                               orig-transport
                               swiss-num)))))
    (make-ocapn-peer 'underlay
                     encoded-designator
                     orig-hints)))

(define (relay-peer->relay-sturdyref relay-peer)
  "Convert RELAY-PEER into an OCapN sturdyref

This sturdyref represents the underlying relay endpoint."
  (let ((netlayer-name (ocapn-peer-transport relay-peer)))
    (unless (eq? netlayer-name 'underlay)
      (error "Attempt to connect to non-relay peer via relay netlayer:"
             netlayer-name)))
  (match (syrup-decode (base32-decode (ocapn-peer-designator relay-peer)))
    ((designator netlayer-name swiss-num)
     (let ((reconstructed-peer
            (make-ocapn-peer netlayer-name
                             designator
                             (ocapn-peer-hints relay-peer))))
       (make-ocapn-sturdyref reconstructed-peer
                             swiss-num)))))


;;; Other utilities
;;; ===============

;; This works as a promise you can fulfill (but not break) multiple times. It's
;; implemented somewhere inbetween a promise, swappable and a ^cell. The initial
;; behavior when no initial value is provided is to have a promise as its
;; default value. When it's fulfilled with a value, that promise is then
;; fulfilled with that given value and then it's "swapped" to the given value.
;; It can also be reset back to the initial behavior. The actor can have its
;; current value (including the initial vow) retrieved by the "current-value"
;; method.
;;
;; It's used below as a sort of promise which can be resolved multiple
;; times.
(define-actor (^swappable-promise bcom #:optional initial-value)
  ;; There are two main reasons we don't want aurie to persist the
  ;; value of this resolver:
  ;; 1. It'll almost always either be an unresolved promise or remote-refr,
  ;;    neither of which are actually depictable (yet).
  ;; 2. This is used in the relay netlayer and should be setup anew each time
  ;;    anyway.
  #:portrait (lambda () '())
  
  (define-values (initial-vow initial-resolver)
    (spawn-promise-and-resolver))

  (define current-value
    (if initial-value
        initial-value
        initial-vow))

  (methods
   ((current-value) current-value)
   ((reset)
    ;; Only reset if we're not already pointing at initial-vow
    (unless (eq? current-value initial-vow)
      (bcom (^swappable-promise bcom))))
   ((fulfill new-value)
    (if (eq? current-value initial-vow)
         (begin
           ($ initial-resolver 'fulfill new-value)
           (bcom (^swappable-promise bcom new-value)))
         (bcom (^swappable-promise bcom new-value))))))

(define-actor (^swappable-forwarder bcom send-to)
  (lambda args
    (apply <- ($ send-to 'current-value) args)))

;; A little utility that maybe, possibly, could be useful for other
;; things and might be worth moving out.  Might be worth supporting
;; both "fulfilled" and "broken" resolutions in that case.
(define (spawn-swappable-promise-and-resolver)
  "Spawn a forwarder, which mostly works like a promise, and a resolver,
which is like a promise resolver which can only be fulfilled, but can
be fulfilled more than once"
  (define send-to (spawn-named 'send-to ^swappable-promise))
  (values (spawn ^swappable-forwarder send-to) send-to))



;;; RELAY CODE
;;; ===========

;; This actually lives *on* the relay vat... ie, it *is* the relay.
;; TODO: This needs to handle all the severance stuff when a disconnect
;;   happens or a new deliver-to is set up
(define-actor (^relay-endpoint _bcom client-session-listener)
  (lambda (their-relay-in)
    (define our-relay-out (spawn ^session-relay-out their-relay-in))
    (define our-relay-in-vow
      (let-on ((client-deliver-incoming
                (<- client-session-listener 'incoming-session our-relay-out)))
        (spawn ^session-relay-in client-deliver-incoming)))
    our-relay-in-vow))

(define-actor (^relay-controller _bcom enliven client-session-listener-resolver)
  (methods
   ;; Connect to TO-ENDPOINT, which can be a live ref or a sturdyref
   ((connect to-endpoint deliver-in)
    ;; TODO: Do we allow deliver-in
    ;; refr or is it always a sturdyref?
    ;; TODO: Do we need to transform this peer?
    (define to-endpoint-vow
      (<- enliven 'enliven (relay-peer->relay-sturdyref to-endpoint)))

    (define our-relay-in
      (spawn ^session-relay-in deliver-in))
    (define their-relay-in-vow
      (<- to-endpoint-vow our-relay-in))

    ;; If our connection to our client severs, we need to propagate that to th
    ;; other relay.
    (when (remote-refr? deliver-in)
      (on-sever deliver-in
                (lambda (type reason)
                  ($ client-session-listener-resolver 'reset)
                  (<-np their-relay-in-vow 'abort))))

    ;; If we loose connection to their prelay, we want to send a sever to the
    ;; client.
    (on their-relay-in-vow
        (lambda (their-relay-in)
          (when (remote-refr? their-relay-in)
            (on-sever their-relay-in
                      (lambda (type reason)
                        (<-np deliver-in 'abort))))))
    
    ;; [Note from cwebber, 2023-07-28:]
    ;; I'm more confident about doing this particular `on' for message
    ;; ordering semantics, though it may have been fine to hand the
    ;; ^session-relay-out their-relay-in-vow... it maybe is!  I'm
    ;; just doing too much at once to carefully analyze for sure,
    ;; and ordering guarantees here is pretty essential
    (define our-relay-out-vow
      (let-on ((their-relay-in their-relay-in-vow))
        ;; Spawn our relay out.
        (spawn ^session-relay-out their-relay-in)))

    ;; Hand back the promise which will allow for sending messages
    ;; once everything is correctly configured
    our-relay-out-vow)
   ((set-session-listener session-listener)
    ($ client-session-listener-resolver 'fulfill session-listener))))

(define (spawn-relay-pair enliven)
  "Spawn a pair of relay objects: the endpoint (public) and controller (private)

ENLIVEN is a capability to enliven a sturdyref, provided with the method 'enliven
and a sturdyref, it should return a promise.
Probably a facet of the MyCapN object.

Returns two values to its continuation, the ENDPOINT and CONTROLLER
respectively."
  (define-values (client-session-listener
                  client-session-listener-resolver)
    (spawn-swappable-promise-and-resolver))

  (values (spawn ^relay-endpoint client-session-listener)
          (spawn ^relay-controller enliven client-session-listener-resolver)))

(define (^session-relay-in bcom deliver-in)
  "Constructs a RELAY-IN object, used to send objects to us"
  (define base-beh
    (methods
     ;; Pass along message to user's deliver-in
     ((deliver encoded-message)
      (<-np deliver-in 'deliver encoded-message))
     ;; Halt the connection.
     ;; TODO: Needs work.
     ((abort)
      (<-np deliver-in 'abort)
      (bcom closed-beh))))
  (define closed-beh
    (lambda _ (error "Relay session closed")))
  base-beh)

(define (^session-relay-out bcom their-relay-in)
  "Constructs a RELAY-OUT object, which we use to send to the other side"
  (define base-beh
    (methods
     ((deliver encoded-message)
      (<-np their-relay-in 'deliver encoded-message))
     ((abort)
      (<-np their-relay-in 'abort)
      (bcom closed-beh))))
  (define closed-beh
    (lambda _ (error "Relay session closed")))
  base-beh)



;;; CLIENT CODE
;;; ===========

;; This lives on the "relay client"

;; We're going to hand the connection establisher, as usual for
;; netlayers, a (blocking) method to read messages and a (blocking)
;; method to write messages.  The connection establisher informs
;; our mycapn of our interest in establishing a new connection,
;; which our mycapn does for us.
;;
;; We're going to do this via two means:
;;
;;  - Incoming messages: An actor which receives messages over
;;    ordinary ocapn abstracted over some other netlayer we're
;;    using.  This actor then sends a message to an intermediary
;;    fibers "inbox" (delivery agent from `(goblins inbox)').
;;    The `read-message' procedure we end up finally setting up
;;    reads from this channel so that the actor can deliver
;;    messages to a blocking interface without itself blocking
;;  - Outgoing messages: These are much easier, as we can simply
;;    serialize the message and fire it off to the remote actor.
(define-actor (^underlay-netlayer bcom self enliven
                                  relay-endpoint-sref-vow
                                  relay-controller-sref-vow)
  (define our-location-vow
    (let-on ((relay-endpoint-sref relay-endpoint-sref-vow))
      (relay-sturdyref->relay-peer relay-endpoint-sref)))

  (define-values (conn-establisher-vow conn-establisher-resolver)
    (spawn-promise-and-resolver))

  (define-values (relay-controller-vow relay-controller-resolver)
    (spawn-swappable-promise-and-resolver))

  ;; We want the relay netlayer to reconnect if a sever occurs with it and its
  ;; server. Do this by attempting a reconnect and backing off if it fails.
  (define* (install-new-relay-controller! #:optional wait-time-sec)
    (define controller-vow
      (if wait-time-sec
          (let-on ((_ (timeout wait-time-sec)))
            (<- enliven 'enliven relay-controller-sref-vow))
          (<- enliven 'enliven relay-controller-sref-vow)))
    (on controller-vow
        (lambda (relay-controller)
          ;; Setup the on-sever to detect when we need to reconnect.
          (when (remote-refr? relay-controller)
            (on-sever relay-controller
                      (lambda (type reason)
                        ($ relay-controller-resolver 'reset)
                        (install-new-relay-controller!))))
          ;; Need to restart listening so we re-register with
          ;; the server since it's a new session.
          (start-listener)
          ($ relay-controller-resolver 'fulfill relay-controller))
        #:catch
        (lambda (err)
          ;; increase the wait time more and more each time.
          (define new-wait-time-sec
            (if wait-time-sec
                (* wait-time-sec 2)
                1))
          ($ relay-controller-resolver 'reset)
          (install-new-relay-controller! new-wait-time-sec))))

  (install-new-relay-controller!)

  (define (start-listener)
    (define (^session-listener _bcom)
      ;; This is where an incoming session comes in...
      (methods
       ((incoming-session session-relay-outgoing)
        (define-values (client-deliver-in incoming-deq-ch incoming-stop?)
          (setup-delivery-agent-and-actor))
        (define message-io
          (spawn-message-io incoming-deq-ch session-relay-outgoing))

        ;; If our connection to the relay server (i.e. session-relay-outgoing)
        ;; severs, we need to inform our CapTP that.
        (when (remote-refr? session-relay-outgoing)
          (on-sever session-relay-outgoing
                    (lambda (type reason)
                      (<-np client-deliver-in 'abort))))

        (<-np conn-establisher-vow message-io #f)
        ;; Now we need to return the client-deliver-in
        client-deliver-in)))
    (define listener (spawn ^session-listener))
    ;; Tell the remote endpoint that we're looking for incoming connections
    (<-np relay-controller-vow 'set-session-listener listener))

  (define-values (setup-netlayer-vow setup-netlayer-resolver)
    (spawn-promise-and-resolver))

  (on our-location-vow
      (lambda (our-location)
        ($ setup-netlayer-resolver 'fulfill (spawn ^netlayer our-location))))

  (define (^netlayer _bcom our-location)
    (methods
     ((netlayer-name) 'underlay)
     ((our-location) our-location)
     ((self-location? loc)
      (same-peer-location? our-location loc))
     ((setup conn-establisher)
      ;; Now that we're set up, transition to the main behavior
      (<-np conn-establisher-resolver 'fulfill conn-establisher))
     ((connect-to remote-peer)
      (define-values (deliver-in incoming-deq-ch incoming-stop?)
        (setup-delivery-agent-and-actor))

      (let-on ((session-relay-outgoing
                (<- relay-controller-vow 'connect remote-peer deliver-in)))
        (define message-io
          (spawn-message-io incoming-deq-ch session-relay-outgoing))

        ;; If our connection to the relay server (i.e. session-relay-outgoing)
        ;; severs, we need to inform our CapTP of that.
        (when (remote-refr? session-relay-outgoing)
          (on-sever session-relay-outgoing
                    (lambda (type reason)
                      (<-np deliver-in 'abort))))

        (<- conn-establisher-vow message-io remote-peer)))))
  (lambda args
    (match args
      [('netlayer-name) 'underlay]
      [args
       (apply <- setup-netlayer-vow args)])))

(define (^relay-netlayer _bcom mycapn controller-sref endpoint-sref)
  "Constructs the relay netlayer which lives on the client.

Takes three arguments at spawn time:
 - MYCAPN: A reference to a MyCapN object configured with the transport netlayer
   to be used for connecting to the relay server. This should be considered to
   be the relay netlayer's own MyCapN and not used for anything else.
 - RELAY-ENDPOINT-SREF: Sturdyref of the endpoint we will communicate with
 - RELAY-CONTROLLER-SREF: Strudyref of the controller which we use to pilot the
   relay we communicate through."
  ;; The not-e2ee relay netlayer
  (define self (spawn ^cell))
  (define underlay-netlayer
    (let ((enliven (facet mycapn 'enliven)))
      (spawn ^underlay-netlayer self enliven controller-sref endpoint-sref)))
  ($ self underlay-netlayer)
  ($ mycapn 'install-netlayer underlay-netlayer)
  (spawn ^noise-netlayer underlay-netlayer #:name 'relay #:mycapn mycapn))

;;; Helpers for the ^relay-controller machinery.
;;; The following code is used for both outgoing and incoming connections
;;; but is used a little bit differently.

;; Set up the client-deliver-in actor as well as the inbox/outbox
;; queues used to communicate between it and the read-message procedure
;; which we will hand to the connection establisher
(define (setup-delivery-agent-and-actor)
  ;; So, the first thing we need to do... let's set up the inbox...
  (define-values (incoming-enq-ch incoming-deq-ch incoming-stop?)
    (spawn-delivery-agent))
  ;; Now we need to set up our delivery proxy which will receive
  ;; messages (since we're going to need to give it to the other
  ;; endpoint)
  (define (^client-deliver-in bcom)
    (define main-beh
      (methods
       ((deliver encoded-message)
        ;; We don't decode the message ourselves at this point, we let
        ;; the other side of the fiber, which has the unmarshallers, do
        ;; that
        (put-message incoming-enq-ch encoded-message)
        ;; No significant value returned (but don't want a zero valued
        ;; continuation error)
        *unspecified*)
       ((abort)
        (put-message incoming-enq-ch the-eof-object)
        (bcom closed-beh))))
    (define closed-beh
      (lambda _ (error "Relay session closed")))
    main-beh)
  (define client-deliver-in (spawn ^client-deliver-in))
  (values client-deliver-in incoming-deq-ch incoming-stop?))

(define (spawn-message-io incoming-deq-ch session-relay-outgoing)
  (define incoming (spawn ^io incoming-deq-ch))
  (define (^message-io _bcom)
    (methods
     [(read-message unmarshallers)
      ($ incoming
         (lambda (ch)
           (match (get-message ch)
             [(? eof-object? eof) eof]
             [msg (syrup-decode msg #:unmarshallers unmarshallers)])))]
     [(write-message msg marshallers)
      (define encoded-msg (syrup-encode msg #:marshallers marshallers))
      (<-np session-relay-outgoing 'deliver encoded-msg)]))
  (spawn ^message-io))

(define relay-env
  (make-persistence-env
   `((((goblins ocapn netlayer relay) ^swappable-forwarder) ,^swappable-forwarder)
     (((goblins ocapn netlayer relay) ^swappable-promise) ,^swappable-promise)
     (((goblins ocapn netlayer relay) ^relay-endpoint) ,^relay-endpoint)
     (((goblins ocapn netlayer relay) ^relay-controller) ,^relay-controller)
     (((goblins ocapn netlayer relay) ^underlay-netlayer) ,^relay-netlayer))
   #:extends (list cell-env facet-env)))
