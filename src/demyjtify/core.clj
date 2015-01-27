(ns demyjtify.core
  (:require [clojure.tools.logging :as log]
            [clojure.tools.trace :as trace]
            [clojure.java.io :as io]
            [clj-logging-config.log4j :as logconf]
            [clj-time.core :as time]
            [clj-time.coerce :as tc]
            [clojure.string :as str]))

(def max-body-chunk
  "Maximum size of a replace-body message to the MTA."
  65535)

(def protocol-version
  "Protocol version number spoken by this library."
  6)

(def minimum-protocol-version
  "Minimum protocol version accepted by this library."
  2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro unless [test & body]
  `(when-not ~test ~@body))

(defmacro unwind-protect [form & body]
  `(try ~form
        (finally ~@body)))

(defmacro ignore-errors [& forms]
  `(try (do ~@forms) (catch Exception e nil)))

(defmacro for-ever [& body]
  `(while true ~@body))

(defn file-size [file]
  (.length (if (string? file)
             (java.io.File. file)
             file)))

(defmacro dprint [& args]
  ;; Why don't the message appear as they should? -wcp25/1/15.
  #_`(log/debug ~@args)
  `(println ~@args))

(defmacro show
  ([variable-name]
   `(dprint ~(str variable-name) "=" ~variable-name))
  ([name & more]
   `(do ~@(map (fn [name] `(show ~name)) (cons name more)))))


(defn socket-reader [socket]
  #_(io/reader socket)
  (.getInputStream socket))

(defn socket-writer [socket]
  #_(io/writer socket)
  (.getOutputStream socket))

(defn map-plist [f plist]
  (map f (take-nth 2 plist) (take-nth 2 (rest plist))))

(defn mapcat-plist [f plist]
  (mapcat f (take-nth 2 plist) (take-nth 2 (rest plist))))

(defn reduce-plist
  "Reduce a property list. F must be a function that accepts three
  arguments: the accumulating result of the reduction, the key, and
  the value associated to the key."
  ([f init plist]
   (reduce (fn [result [k v]]
             (f result k v))
           init (map-plist vector plist)))
  ([f plist]
   (reduce (fn [result [k v]]
             (f result k v))
           (map-plist vector plist))))

(defn plist->map
  "Convert a property list to a map."
  [plist]
  (apply hash-map plist)
  ;; use hash-map instead
  #_(reduce-plist (fn [result k v] (assoc result k v)) {} plist))

(defn readable? [obj]
  (instance? Readable obj))

(defn file? [obj]
  (instance? java.io.File obj))

(defn reader [obj]
  (if (instance? java.net.Socket obj)
    (socket-reader obj)
    obj))

(defn writer [obj]
  (if (instance? java.net.Socket obj)
    (socket-writer obj)
    obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti send-action-imp
  "Write on SOCKET a byte sequence (a packet) representing the
  ACTION."
  (fn [action context] (:action action)))

(defmulti default-event-handler
  (fn [event context] (:event event)))

(defn send-action
  "Trace the call and delegate to SEND-ACTION-IMP."
  [action context]
  (dprint "<<" action)
  (send-action-imp action context))

(defn handle-event
  "Handle an MTA EVENT in that CONTEXT."
  [event context]
  (dprint ">>" event)
  (if-let [call-back (get-in context [:handlers (event :event)])]
    (call-back event context)
    (default-event-handler event context)))

(defmacro error [type & args]
  `(throw (ex-info "Protocol error" (hash-map :type '~type ~@args))))

#_(defmacro do1 [form & rest]
    `(let [value# ~form]
       ~@rest
       value#))

(defn chunking [sequence size]
  (letfn [(make [s]
            (lazy-seq
             (when (seq s)
               (cons (take size s)
                     (make (drop size s))))))]
    (make sequence)))

;; does not include the separator
(defn chunk-by
  [f sequence]
  (letfn [(make [s]
            (lazy-seq
             (when (seq s)
               (loop [in s
                      out []]
                 (cond
                   (empty? in) (cons out (make in))
                   (f (first in)) (cons out (make (rest in)))
                   :else (recur (rest in) (conj out (first in))))))))]
    (make sequence)))

(defn split-by
  "Like SPLIT-WITH but do not include the separators"
  [f sequence]
  (let [[head tail] (split-with f sequence)]
    [head (rest tail)]))

(defn decode-int [bytes]
  (reduce (fn [val byte] (bit-or (bit-shift-left val 8) byte)) 0 bytes))

(defn encode-int
  "Code VALUE integer in a sequence of N bytes in network byte
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

(defn decode-packet-data
  "Decode the packet BYTES according to its FORMAT-DESCRIPTION,
  splitting it in its subarts (fields).  FORMAT-DESCRIPTION is a
  list of keywords among:

  :CHAR		a character
  :C-STRING	a NULL terminated string (the null is stripped)
  :C-STRINGS	a sequence of :C-STRING
  :INT16	a two byte integer in NBO
  :INT32	a four byte integer in NBO

  If data is not laid according to FORMAT-DESCRIPTION the result is
  unknown."
  [bytes format-description]
  (letfn [(split-at-null [data]
            (split-with #(not (zero? %)) data))
          (bytes-to-string [data]
            (str/join (map char data)))]
    (loop [data bytes
           elements format-description
           result []]
      (if (empty? elements)
        result
        (case (first elements)
          :char (recur (rest data) (rest elements)
                       (conj result (char (first data))))
          :c-string (let [[span after] (split-at-null data)]
                      (recur (rest after) (rest elements)
                             (conj result (bytes-to-string span))))
          :c-strings (conj result (map bytes-to-string (chunk-by #(zero? %) data)))
          :int16 (recur (drop 2 data) (rest elements)
                        (conj result (decode-int (take 2 data))))
          :int32 (recur (drop 4 data) (rest elements)
                        (conj result (decode-int (take 4 data))))
          (error unknown-element-type :element (first elements)))))))

(defmacro with-packet-data
  "Execute BODY within a lexical context that binds FIELDS to the
  exploded DATA packet.  DATA is a byte sequence and FIELDS is a
  list of pairs; each pair is a variable name and a type specifier.
  See DECODE-PACKET-DATA for further details about type
  specifiers."
  [data fields & body]
  `(let [~(vec (take-nth 2 fields))
         (decode-packet-data ~data '~(take-nth 2 (rest fields)))]
     ~@body))

(defn read-byte
  "Read a single byte from STREAM."
  [input]
  (let [b (.read input)]
    (assert (<= 0 b 255))
    b))

;; lazy version
#_(defn read-bytes [input len]
    (lazy-seq
     (when (pos? len)
       (let [b (read-byte input)]
         (unless (neg? b)
           (cons b (read-bytes input (dec len))))))))

(defn read-bytes [input len]
  (loop [n len
         bytes []]
    (if (zero? n)
      bytes
      (let [b (read-byte input)]
        (recur (dec n) (conj bytes b))))))

(defn lazy-slurp [readable]
  (take-while pos? (repeatedly #(read-byte readable))))

(defn receive-byte [input]
  (read-byte input))

(defn unsign-byte [b]
  (if (neg? b)
    (+ 256 b)
    b))

#_(defn receive-sequence [len input]
    (read-bytes input len))

(defn receive-sequence [len input]
  (let [buf (byte-array len)]
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
  "Receive an MTA event from INPUT.  Return the corresponding
  MTA-EVENT object."
  [context]
  (let [[command data] (receive-packet (reader (context :socket)))]
    (case command
      \A {:event :abort}
      \B {:event :body :data data}
      \C (with-packet-data data [host-name :c-string
                                 family :char
                                 port :int16
                                 address :c-string]
           {:event :connect
            :host-name host-name :family family :port port :address address})
      \D (with-packet-data data [command :char
                                 namevals :c-strings]
           {:event :define-macros
            :command command
            :definitions (letfn [(strip-brackets [string]
                                   (if (and (> (count string) 2)
                                            (= (first string) \{))
                                     (str/join (butlast (rest string)))
                                     string))]
                           (plist->map
                            (mapcat-plist (fn [name value]
                                            (list (strip-brackets name) value))
                                          namevals)))})
      \E {:event :end-of-message}
      \H {:event :hello
          :greeting (first (decode-packet-data data '(:c-string)))}
      \K {:event :disconnect}
      \L (with-packet-data data [name :c-string
                                 value :c-string]
           {:event :header
            :name name :value value})
      \M (with-packet-data data [sender :c-string
                                 options :c-strings]
           {:event :mail
            :sender sender :options options})
      \N {:event :end-of-headers}
      \O (with-packet-data data [version :int32
                                 actions :int32
                                 protocol-mask :int32]
           {:event :options
            :version version :actions actions :protocol-mask protocol-mask})
      \Q {:event :quit}
      \R (with-packet-data data [address :c-string
                                 options :c-strings]
           {:event :recipient
            :address address :options options})
      \T {:event :data}
      \U {:event :unknown
          :command (first (decode-packet-data data '(:c-string)))})))

(defn get-macro
  "Find in CONTEXT the value of the macro with name NAME.  The macro
  can be associated to any command."
  [context name]
  (first (filter (fn [[command macros]]
                   (get macros name))
                 (context :macros))))

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

(defn action-mask [action]
  (action-masks action))

(defn event-mask [event]
  (event-masks event))

(defn actions-mask
  "Compose a bitmask corresponding to the list of ACTIONS."
  [actions]
  (reduce bit-or 0 (map action-mask actions)))

(defn protocol-events-mask
  "Compose the bitmask corresponding to the list of protocol EVENTS."
  [events]
  (reduce bit-or 0 (map event-mask events)))

(defn bitmask->set
  "Return the list of keywords taken from MAP corresponding to the
  bitmask MASK."
  [mask map]
  (reduce-kv (fn [result key value]
               (if (zero? (bit-and value mask))
                 result
                 (conj result key)))
             #{} map))

(defn events-from-mask
  "Return the list of event keywords corresponding to the bitmask
  MASK."
  [mask]
  (bitmask->set mask event-masks))

(defn actions-from-mask
  "Return the list of action keywords corresponding to the bitmask
  MASK."
  [mask]
  (bitmask->set mask action-masks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (let [required-events-mask (protocol-events-mask (context :events))
        optional-events-mask (protocol-events-mask (context :optional-events))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmacro assert-action [action]
  `(assert (get-in ~'context [:actions ~action])))

(defmacro assert-event [event]
  `(assert (get-in ~'context [:events ~event])))

(defmacro defaction [type & body]
  `(defmethod send-action-imp ~type
     [~'action ~'context]
     (let [~'socket (writer (~'context :socket))]
       ~@body)))

(defmacro defaction-a [type & body]
  `(defaction ~type
     (assert-action ~type)
     ~@body))

(defaction-a :add-recipient
  (if (action :parameters)
    (send-packet socket \2 (action :address) 0 (action :parameters) 0)
    (send-packet socket \+ (action :address) 0)))

(defaction-a :delete-recipient
  (send-packet socket \- (action :address) 0))

(defaction :accept
  (send-packet socket \a))

(defaction-a :replace-body
  (let [socket socket
        body (action :body)]
    (letfn [(send-sequence [seq]
              (doseq [bytes (chunking seq max-body-chunk)]
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

(defaction-a :change-sender
  (if (action :parameters)
    (send-packet socket \e (action :address) 0 (action :parameters) 0)
    (send-packet socket \e (action :address) 0)))

(defaction-a :add-header
  (if (action :position)
    (send-packet socket \i (action :position) 0 (action :name) 0 (action :value) 0)
    (send-packet socket \h (action :name) 0 (action :value) 0)))

(defaction-a :change-header
  (send-packet socket \m (encode-int32 (action :index))
               (action :name) 0 (action :value) 0))

(defaction-a :quarantine
  (send-packet socket \q (action :reason) 0))

(defaction :reject
  (send-packet socket \r))

(defaction :skip
  (assert-event :can-skip)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn server-loop
  "Run the milter protocol loop using CONTEXT as context.  It returns
  on a MILTER-CONDITION or if any event handler returns NIL instead
  of a MILTER-ACTION object."
  [context]
  (letfn [(process-event [ctx]
            (try
              (if-let [event (receive-event ctx)]
                (handle-event event ctx))
              (catch clojure.lang.ExceptionInfo e
                (throw (ex-info "Milter error" (assoc (ex-data e) :context ctx) e)))
              (catch Exception e
                (throw (ex-info "Internal error" {:context ctx :exception e} e)))))]
    (try
      (let [ctx (loop [ctx context]
                  (if-let [c (process-event ctx)]
                    (recur c)
                    ctx))]
        (dprint "Exiting normally server loop")
        (handle-event {:event :disconnect} ctx))
      (catch clojure.lang.ExceptionInfo e
        (let [ex ((ex-data e) :exception)]
          (log/error e "Exiting ABNORMALLY server loop" ex)
          (clojure.stacktrace/print-stack-trace ex 10))
        (handle-event {:event :disconnect} ((ex-data e) :context)))
      (catch Exception e
        (log/error e "Exiting ABNORMALLY server loop" e)
        (handle-event {:event :disconnect} context)))))

(defn start-milter
  "Start the milter and enter an endless loop serving connections from
  the MTA.  On client connections ON-CONNECT is called passing a
  context object.  The task of the ON-CONNECT function is to return a
  context object which is passed to the mail server-loop in a separate
  thread."
  [port on-connect]
  (with-open [ss (java.net.ServerSocket. port)]
    (for-ever
     (let [cs (.accept ss)]
       (dprint "Received connection from MTA" cs)
       (future
         (server-loop (on-connect {:socket cs})))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn log-formatter [log]
  (println (str (tc/to-date (log :timeStamp)))
           (str "[" (log :threadName) "]")
           (log :message))
  (flush))

(defn enable-tracing []
  (trace/trace-ns 'demyjtify.core)
  ;; no need to also trace the loggin function
  (trace/untrace-var* 'demyjtify.core/log-formatter))

(defn setup-logging []
  (logconf/set-logger! :name "console"
                       :level :debug
                       :append true
                       :out log-formatter))

(defn debug [& args]
  (let [port (if (empty? args)
               4242
               (first args))]
    (enable-tracing)
    (setup-logging)
    (log/info "Strating server on port" port)
    ;; don't hang waiting the milter to return so that we still have the REPL
    (future (start-milter port identity))))
