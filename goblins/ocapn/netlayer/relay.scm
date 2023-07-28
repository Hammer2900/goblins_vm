;;; Copyright 2023 Christine Lemmer-Webber
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
  #:use-module (goblins core)
  #:use-module (goblins vat)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib queue)
  #:use-module (goblins actor-lib facet)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins utils base32)
  #:use-module (goblins contrib syrup)
  #:export (relay-sturdyref->relay-machine
            relay-machine->relay-sturdyref)
  )



;;; URI utils
;;; =========

(define (relay-sturdyref->relay-machine relay-endpoint-sref)
  "Convert RELAY-ENDPOINT-REF into an ocapn machine"
  (let* ((swiss-num (ocapn-sturdyref-swiss-num relay-endpoint-sref))
         (machine (ocapn-sturdyref-machine relay-endpoint-sref))
         (orig-transport (ocapn-machine-transport machine))
         (orig-address (ocapn-machine-address machine))
         (orig-hints (ocapn-machine-hints machine))
         (encoded-address
          (base32-encode
           (syrup-encode (list orig-address 
                               orig-transport
                               swiss-num)))))
    (make-ocapn-machine 'relay
                        encoded-address
                        orig-hints)))

(define (relay-machine->relay-sturdyref relay-machine)
  (let ((netlayer-name (ocapn-machine-transport relay-machine)))
    (unless (eq? netlayer-name 'relay)
      (error "Attempt to connect to non-relay machine via relay netlayer:"
             netlayer-name)))
  (match (syrup-decode (base32-decode (ocapn-machine-address relay-machine)))
    ((address netlayer-name swiss-num)
     (let ((reconstructed-machine
            (make-ocapn-machine netlayer-name
                                address
                                (ocapn-machine-hints relay-machine))))
       (make-ocapn-sturdyref reconstructed-machine
                             swiss-num)))))


;;; RELAY CODE
;;; ==========

;; This actually lives *on* the relay vat... ie, it *is* the relay.
;; TODO: This needs to handle all the severance stuff when a disconnect
;;   happens or a new deliver-to is set up
(define (spawn-relay-pair enliven)
  (define-cell client-session-listener #f)
  (define (^relay-endpoint bcom)
    (methods
     ((incoming-session their-relay-incoming)
      (unless ($ client-session-listener)
        (error "Relay endpoint not configured!"))
      (define our-relay-outgoing (spawn ^session-relay-out 'TODO))
      (define our-relay-incoming-vow
        (on (<- client-session-listener 'new-session )
            (lambda ())
            #:promise? #t)
        (setup-eventual-replay-incoming their-repay-incoming our-replay-outgoing))
      (define-values (our-relay-incoming our-relay-outgoing)
        'TODO)
      our-relay-incoming-vow)))
  (define (^relay-controller _bcom)
    (methods
     ((set-session-listener new-session-listener)
      ($ deliver-to new-session-listener))
     ;; Connect to TO-ENDPOINT, which can be a live ref or a sturdyref
     ((connect to-endpoint deliver-in)
      ;; TODO: Do we allow deliver-in to either be a sturdyref or a live
      ;; refr or is it always a sturdyref?
      (define to-endpoint-vow (enliven to-endpoint))
      (define our-relay-incoming 'TODO)
      ;; We can't complete the outgoing relay until we 
      (define our-relay-outgoing-vow
        (on (<- to-endpoint-vow 'incoming-session our-relay-incoming)
            (lambda (their-relay-incoming)
              'TODO)
            #:promise? #t))
      ;; hand back the promise which will allow for sending messages
      our-relay-outgoing-vow)))
  (values (spawn ^relay-endpoint)
          (spawn ^relay-controller)))

(define (^session-relay-in bcom deliver-in)
  (methods
   ;; Pass along message to user's deliver-in
   ((deliver encoded-message)
    (<-np deliver-in 'deliver encoded-message))
   ;; Halt the connection.
   ;; TODO: Needs work.
   ((abort)
    (error "TODO: abort behavior not implemented"))))

(define (^session-relay-out bcom)
  'TODO)



;;; CLIENT CODE
;;; ===========

;; This lives on the "relay client"
(define (^relay-netlayer bcom enliven relay-endpoint-sref relay-controller)
  (define our-location
    (relay-sturdyref->relay-machine relay-endpoint-sref))

  ;; Spawns the deliver-out code, the deliver-in code,
  ;; as well as the reader and the writer wrapping them to deliver back
  ;; to mycapn via the conn-establisher
  (define (spawn-client-delivery-agents conn-establisher)
    ;; The asynchronous inbox

    (define (^deliver-out _bcom)
      'TODO)
    (define (^deliver-in _bcom)
      'TODO)


    (values deliver-out deliver-in
            reader writer))

  (define (start-listener conn-establisher)
    (define (^session-listener _bcom)
      ;; This is where an incoming session comes in...
      (lambda (relay-outgoing)
        ;; So we need to spawn the delivery agent here then wrap the
        ;; two procedures...
        (<-np conn-establisher
              'TODO-READER
              'TODO-WRITER
              ;; TODO: update with jessica's new code... wtf do we do
              ;; here again... something hand over the machine or etc???
              ;; Does that need to be delivered as an arugment to the session-listener?
              #t)
        ;; Now we need to head back the client-deliver-incoming
        
        ))
    (define listener (spawn ^session-listener))
    ;; Tell the remote endpoint that we're looking for incoming connections
    (<-np relay-controller 'set-session-listener listener))

  (define base-beh
    (methods
     ((netlayer-name) 'relay)
     ((our-location) our-location)))
  (define pre-setup-beh
    (extend-methods base-beh
      ((setup conn-establisher)
       (start-listener conn-establisher)
       ;; Now that we're set up, transition to the main behavior
       (bcom (ready-beh conn-establisher)))))
  (define (ready-beh conn-establisher)
    (extend-methods base-beh
      ((self-location? loc)
       (same-machine-location? our-location loc))
      ((connect-to remote-machine)
       ;;;; Commented out because the relay is going to do some key authentication
       ;;;; checks later so it needs the machine itself... but we're going to need
       ;;;; need to do that too presumably, so maybe we'll uncomment this once we
       ;;;; add cryptography
       ;; ;; Now we use our relay controller to connect to the object
       ;; ;; First that means deconstructing the URI so we can get out
       ;; ;; the relevant sturdyref object
       ;; (define remote-endpoint-sref (relay-machine->relay-sturdyref remote-machine))
       ;; ;; Now we need to enliven it
       ;; (define remote-endpoint-vow (enliven remote-endpoint-sref))
       
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
       ;;
       ;; So, the first thing we need to do... let's set up the inbox...
       (define-values (incoming-enq-ch incoming-deq-ch incoming-stop?)
         (spawn-delivery-agent))
       ;; Now we need to set up our delivery proxy which will receive
       ;; messages (since we're going to need to give it to the other
       ;; endpoint)
       (define (^deliver-in _bcom)
         (methods
          ((deliver encoded-message)
           ;; We don't decode the message ourselves at this point, we let
           ;; the other side of the fiber, which has the unmarshallers, do
           ;; that
           (put-message incoming-enq-ch encoded-message))
          ((abort)
           (error "TODO: abort behavior not implemented"))))
       (define deliver-in (spawn ^deliver-in))

       ;; TODO: Should we be giving just the sturdyref to the endpoint
       ;;   or the remote-machine?  I'm not sure.
       (on (<- relay-controller 'connect deliver-in remote-machine)
           (lambda (session-relay-outgoing)
             ;; These two procedures are actually used by the syscaller-free fibers
             ;; that the mycapn / conn-establisher set up which run in their own
             ;; loop.  So this is *not* happening in a Goblins context, and these
             ;; two will block
             (define (read-message unmarshallers)
               (define encoded-message (get-message incoming-deq-ch))
               (syrup-decode #:unmarshallers unmarshallers))
             (define (write-message msg marshallers)
               (define encoded-msg (syrup-encode msg #:marshallers marshallers))
               (<-np-extern session-relay-outgoing encoded-msg))
             (<- conn-establisher
                 read-message
                 write-message
                 ;; TODO: Switch to Jessica's latest code here
                 #f))
           #:promise? #t))))
  pre-setup-beh)


