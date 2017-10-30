(ns demyjtify.actions
  (:require [clojure.java.io :as io]
            [bnb4clj.core :refer :all]
            [demyjtify.util :refer :all]))

(def action-masks
  "Map of action bitmasks lifted from <libmilter/mfapi.h>."
  {:add-header  	0x001
   :change-body         0x002
   :add-recipient	0x004
   :delete-recipient	0x008
   :change-header	0x010
   :quarantine          0x020
   :change-sender	0x040
   :add-recipient-par	0x080
   :choose-macros	0x100})

(defn action-mask [action]
  (action-masks action))

(defn actions-mask
  "Compose a bitmask corresponding to the list of ACTIONS."
  [actions]
  (reduce bit-or 0 (map action-mask actions)))

(defn actions-from-mask
  "Return the list of action keywords corresponding to the bitmask
  MASK."
  [mask]
  (bitmask->set mask action-masks))

(defmulti send-action-imp
  "Write on SOCKET a byte sequence (a packet) representing the
  ACTION."
  (fn [action context] (:action action)))

(defn send-action
  "Trace the call and delegate to SEND-ACTION-IMP."
  [action context]
  (dprint "<<" action)
  (send-action-imp action context))



(defn send-byte
  "Write BYTE to STREAM.  Signal MILTER-BROKEN-COMMUNICATION if
  it can't."
  [byte stream]
  (assert (<= 0 byte 255))
  (.write stream byte))

;; not sure it does what I mean -wcp17/1/15.
(defn finish-output [stream]
  (.flush stream))

(defn send-sequence
  "Write a SEQUENCE of bytes to STREAM.  Signal an error if it
  can't."
  [sequence stream]
  (.write stream (byte-array sequence)))

(defn encode-int
  "Encode VALUE integer into a sequence of LEN bytes in network byte
  order."
  [value len]
  (reverse (take len
                 (map (partial bit-and 0xff)
                      (iterate #(bit-shift-right % 8)
                               value)))))

(defn encode-int32 
  "Code VALUE integer in a sequence of 4 bytes in network byte
  order."
  [value]
  (encode-int value 4))

(defn send-packet
  "Send a protocol packet to the MTA through STREAM.  DATA is a
  sequence of objects of different nature.  Try to convert them to
  byte sequences before sending them down the socket."
  [stream & data]
  (letfn [(data-length [list]
            (reduce + 0
                    (map (fn [item]
                           (cond
                             (string? item) (count item)
                             (list? item) (count item)
                             (seq? item) (count item)
                             (integer? item) 1
                             (char? item) 1
                             :else (error invalid-type :object (type item))))
                         list)))
          (write-data [data stream]
            (doseq [item data]
              (cond
                (string? item) (send-sequence (map int item) stream)
                (list? item) (send-sequence (map int item) stream)
                (seq? item) (send-sequence (map int item) stream)
                (integer? item) (send-byte item stream)
                (char? item) (send-byte (int item) stream)
                :else (error invalid-type :object (type item)))))]
    (let [len (data-length data)]
      (dprint "SENDING" len "bytes packet," (count data) "items")
      (write-data (cons (encode-int32 len) data) stream)
      (finish-output stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defaction [type & body]
  `(defmethod send-action-imp ~type
     [~'action ~'context]
     (let [~'socket (writer (~'context :socket))
           ~'assert-action (fn []
                             ;; Make sure the action we are about to perform is allowed and
                             ;; expected by the MTA.  Some actions need to be agreed beforehand
                             ;; during the initial handshake.  See the :OPTIONS event.
                             (assert (get-in ~'context [:actions ~type])))
           ]
       ~@body)))

(defaction :add-recipient
  (assert-action)
  (if (action :parameters)
    (send-packet socket \2 (action :address) 0 (action :parameters) 0)
    (send-packet socket \+ (action :address) 0)))

(defaction :delete-recipient
  (assert-action)
  (send-packet socket \- (action :address) 0))

(defaction :accept
  (send-packet socket \a))

(defaction :replace-body
  (assert-action)
  (let [socket socket
        body (action :body)]
    (letfn [(send-sequence [seq]
              (doseq [bytes (partition max-body-chunk seq)]
                (send-packet socket \b bytes)))
            (send-readable [readable]
              (send-sequence (lazy-slurp readable)))
            (send-file [file]
              (with-open [input (io/reader file)]
                (send-readable input)))]
      (cond
        (string? body) (send-sequence (seq body))
        (seq? body) (send-sequence body)
        (readable? body) (send-readable body)
        (file? body) (send-file body)))))

(defaction :continue
  (send-packet socket \c))

(defaction :discard
  (send-packet socket \d))

(defaction :change-sender
  (assert-action)
  (if (action :parameters)
    (send-packet socket \e (action :address) 0 (action :parameters) 0)
    (send-packet socket \e (action :address) 0)))

(defaction :add-header
  (assert-action)
  (if (action :position)
    (send-packet socket \i (action :position) 0 (action :name) 0 (action :value) 0)
    (send-packet socket \h (action :name) 0 (action :value) 0)))

(defaction :change-header
  (assert-action)
  (send-packet socket \m (encode-int32 (action :index))
               (action :name) 0 (action :value) 0))

(defaction :quarantine
  (assert-action)
  (send-packet socket \q (action :reason) 0))

(defaction :reject
  (send-packet socket \r))

(defaction :skip
  ;; Not sure to have really understood the meaning of the events
  ;; flag :CAN-SKIP -wcp27/1/15.
  (assert (get-in context [:events :can-skip]))
  (send-packet socket \s))

(defaction :temporary-failure
  (send-packet socket \t))

(defaction :reply-code
  (send-packet socket \y
               (format "%03d %s\000" (action :smtp-code) (action :text))))

(defaction :options
  (send-packet socket \O
               (encode-int32 (action :version))
               (encode-int32 (action :actions))
               (encode-int32 (action :protocol-mask))))

