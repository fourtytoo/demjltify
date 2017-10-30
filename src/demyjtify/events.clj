(ns demyjtify.events
  (:require [clojure.java.io :as io]
            [bnb4clj.core :refer :all]
            [demyjtify.util :refer :all]
            [demyjtify.parser :refer :all]
            [demyjtify.actions :refer [send-action actions-mask actions-from-mask]]))


(defn receive-byte [input]
  (read-byte input))

#_(defn receive-sequence [len input]
    (read-bytes input len))

(defn receive-sequence [len input]
  (let [buf (byte-array len)
        unsign-byte (fn [b]
                      (if (neg? b)
                        (+ 256 b)
                        b))]
    (loop [start 0]
      (when (> len start)
        (let [n (.read input buf start (- len start))]
          (recur (+ start n)))))
    ;; don't know why but byte arrays are signed
    (map unsign-byte (seq buf))))

(defn receive-int32
  "Read a 32bit integer in network byte order (big-endian) from STREAM."
  [socket]
  (decode-int (receive-sequence 4 socket)))

(defn receive-packet
  "Receive a protocol packet from the MTA through STREAM.  Return
  two values the command (a character) and its data (a byte
  sequence)."
  [input]
  (let [len (dec (receive-int32 input))
        command (char (receive-byte input))
        data (receive-sequence len input)]
    (dprint "RECEIVED command" command "and" len "bytes of data")
    [command data]))

(defn receive-event
  "Receive an MTA event, parse it, and return the clojure structure."
  [context]
  (let [[command data] (receive-packet (reader (context :socket)))]
    (parse-packet command data)))

(defn get-macro
  "Find in CONTEXT the value of the macro with name NAME.  The macro
  can be associated to any command."
  [context name]
  (first (filter (fn [[command macros]]
                   (get macros name))
                 (context :macros))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; There would be many more flags in mfdef.h but their meaning is, at
;; best, unclear.
(def event-masks
  "Map of event bitmasks lifted from <libmilter/mfdef.h>."
  {:connect			0x0001
   :hello			0x0002
   :mail			0x0004
   :recipient			0x0008
   :body			0x0010
   :header			0x0020
   :end-of-headers		0x0040
   :reply-headers		0x0080
   :unknown			0x0100
   :data			0x0200
   ;; Unless I misunderstood the almost inexistent documentation this
   ;; is no event.  The SKIP flag means that the MTA accepts skip
   ;; actions (in reply to BODY events).  Which means this should
   ;; have ended up in the actions mask, not the events one
   ;; -wcp20/12/11.
   :can-skip			0x0400
   ;; MTA should also send RCPT commands that have been rejected
   ;; because the user is unknown or invalid.
   :rejected-recipients 	0x0800})

(def all-events-mask
  (reduce bit-or (vals event-masks)))

(defn event-mask [event]
  (get event-masks event 0))

(defn protocol-events-mask
  "Compose the bitmask corresponding to the list of protocol EVENTS."
  [events]
  (reduce bit-or 0 (map event-mask events)))

(defn events-from-mask
  "Return the list of event keywords corresponding to the bitmask
  MASK."
  [mask]
  (bitmask->set mask event-masks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti default-event-handler
  (fn [event context] (:event event)))

(defn handle-event
  "Handle an MTA EVENT in that CONTEXT."
  [event context]
  (dprint ">>" event)
  (if-let [call-back (get-in context [:handlers (event :event)])]
    (call-back event context)
    (default-event-handler event context)))

;; This macro intentionally captures three variables CONTEXT, EVENT,
;; and CALL-BACK.
(defmacro defhandler [type & body]
  `(defmethod default-event-handler ~type
     [~'event ~'context]
     ~@body))

(defhandler :default
  (dprint "unhandled event" event)
  (send-action {:action :continue} context)
  context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This simple function will take care of the necessary bookkeeping
;; without writing all that baroque infrastructure found inside
;; libmilter.  This works because macros are notified to the milter
;; just before the command they belong to.  The validity of those
;; macros spans until the next same command or a prerequisite command.
;; Here CONTEXT contains a map (:MACROS) where to each command another
;; map is associated.
;; 
;; Here is a sample context:
;; {:socket ...,
;;  :events ...,
;;  :macros {\M {"rcpt_host" "foo.bar.com", ...}, ...}, ...}
;;
(defhandler :define-macros
  (let [command (event :command)
        definitions (event :definitions)
        macros (drop-while (fn [cmd]
                             (not (= command (first cmd))))
                           (context :macros))]
    (dprint "define macros for command" command ": " definitions)
    ;; no action needs to be performed
    (assoc context :macros
           (cons [command definitions]
                 (if (empty? macros)
                   (context :macros)
                   macros)))))

(defmacro show-event-mask [var]
  `(dprint ~(str var " =")
           (events-from-mask ~var)))

(defmacro show-action-mask [var]
  `(dprint ~(str var " =")
           (actions-from-mask ~var)))

(defhandler :options
  (let [optional-events-mask (protocol-events-mask (context :optional-events))
        required-events-mask (protocol-events-mask
                              (clojure.set/difference (keys (context :handlers))
                                                      (context :optional-events)))
        required-actions-mask (actions-mask (context :actions))
        optional-actions-mask (actions-mask (context :optional-actions))
        mta-provided-events (event :protocol-mask)
        mta-allowed-actions (event :actions)
        common-events-mask (bit-and mta-provided-events
                                    (bit-or required-events-mask optional-events-mask))
        common-actions-mask (bit-and mta-allowed-actions
                                     (bit-or required-actions-mask optional-actions-mask))]
    (show-event-mask required-events-mask)
    (show-event-mask optional-events-mask)
    (show-event-mask mta-provided-events)
    (show-event-mask common-events-mask)
    (show-action-mask required-actions-mask)
    (show-action-mask optional-actions-mask)
    (show-action-mask mta-allowed-actions)
    (show-action-mask common-actions-mask)
    (when (< (event :version) minimum-protocol-version)
      (error wrong-protocol-version
             :mta-version (event :version)
             :milter-version protocol-version))
    (unless (= required-events-mask
               (bit-and common-events-mask required-events-mask))
      (error events-not-supported
             :unavailable-events (events-from-mask
                                  (bit-and (bit-not mta-provided-events) required-events-mask))))
    (unless (= required-actions-mask
               (bit-and common-actions-mask required-actions-mask))
      (error actions-not-allowed
             :forbidden-actions (actions-from-mask
                                 (bit-and (bit-not mta-allowed-actions) required-actions-mask))))
    (send-action {:action :options
                  :version (min protocol-version (event :version))
                  :actions common-actions-mask
                  ;; Not sure, but looks like here we tell the MTA
                  ;; what event we do _not_ want.
                  :protocol-mask
                  (bit-and mta-provided-events
                           ;; we mask out some flags as these control
                           ;; some useless aspects of the milter
                           ;; protocol such as no-reply events
                           all-events-mask
                           (bit-not common-events-mask))}
                 context)
    ;; Update the events and action slots to reflect what we agree
    ;; with the MTA.
    (-> context
        (assoc :events (let [events (events-from-mask common-events-mask)]
                         ;; if MTA protocol is older than version 5, MTA
                         ;; always expects a reply after each header
                         (if (< (event :version) 5)
                           (cons :reply-headers events)
                           events)))
        (assoc :actions (actions-from-mask common-actions-mask)))))

(defhandler :end-of-message
  (send-action {:action :accept} context)
  ;; Make sure we don't leave behind macros defined in this message.
  (assoc context :macros {}))

(defhandler :quit
  ;; this will cause the interruption of the server loop
  nil)

;; This is a synthetic event generated by the milter library itself.
;; The only thing a milter is supposed to do here is to close the
;; socket, but the call back here just in case.  The return value is
;; irrelevant.
(defhandler :disconnect
  (.close (context :socket))
  nil)

(defhandler :abort
  ;; No action required, but make sure we don't carry over macros
  ;; definied before.
  (assoc context :macros {}))

(defhandler :header
  ;; Send a confirmation only if the MTA expects it.
  (when (get-in context [:events :reply-headers])
    (send-action {:action :continue} context))
  context)

