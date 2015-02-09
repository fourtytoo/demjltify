(ns fourtytoo.demyjtify.snoop
  (:require [clojure.tools.logging :as log])
  (:use [fourtytoo.bnb4clj]
        [fourtytoo.demyjtify.util]
        [fourtytoo.demyjtify.events]
        [fourtytoo.demyjtify.actions])
  (:import
   [java.net InetSocketAddress]
   [java.nio ByteBuffer CharBuffer]
   [java.nio.channels ServerSocketChannel SocketChannel Selector SelectionKey]
   [java.nio.charset Charset]))


(defn make-selector []
  (java.nio.channels.Selector/open))

(defn channel-blocking! [channel blocking?]
  (.configureBlocking channel blocking?))

(defn open-client-socket-channel [address]
  "Return a new channel connected to address."
  (SocketChannel/open address))

(defn socket-address [host port]
  (InetSocketAddress. host port))

(defn register-channel
  "Register CHANNEL with SELECTOR.  INTEREST an inclusive or of one or
  more: OP_READ, OP_WRITE, OP_ACCEPT, OP_CONNECT."
  [selector channel interest]
  (.register channel selector interest))

(defn make-channels-selector [channels]
  (let [selector (make-selector)]
    (doseq [[channel interest] channels]
      (register-channel selector channel interest))
    selector))

(defn select-on-channels [channels timeout]
  (let [selector (make-channels-selector channels)]
    (cond (not timeout) (.select selector)
          (zero? timeout) (.selectNow selector)
          :else (.select timeout))
    (.selectedKeys selector)))

(defn open-server-socket-channel [port]
  (let [channel (ServerSocketChannel/open)
        _ (.configureBlocking channel false)
        server-socket (.socket channel)
        inet-socket-address (InetSocketAddress. port)]
    (.bind server-socket inet-socket-address)
    channel))

(defmacro with-channel-connections [[channel port] & body]
  `(with-open [server-channel# (open-server-socket-channel ~port)]
     (for-ever
      (let [~channel (.accept server-channel#)]
        ~@body))))

(defn channel-socket [channel]
  (.socket channel))

(defn inet-address [host port]
  (InetSocketAddress. host port))

(defmacro with-open-channel [[channel host port] & body]
  `(with-open [~channel (open-client-socket-channel (inet-address ~host ~port))]
     ~@body))

(defn select-keys [channels timeout]
  (lazy-seq
   (concat (select-on-channels channels timeout)
           (select-keys channels timeout))))

(defn eavesdrop-milter
  [mta-port milter-port]
  (with-channel-connections [mta-channel mta-port]
    (println "connection from MTA (" mta-channel ")") (finish-output)
    (with-open-channel [milter-channel "localhost" milter-port]
      (channel-blocking! milter-channel false)
      (println "connected to milter (" milter-channel ")") (finish-output)
      (loop [keys (select-keys (map (fn [ch]
                                      [ch (bit-or SelectionKey/OP_READ
                                                  SelectionKey/OP_WRITE)])
                                    (list mta-channel milter-channel)) 
                               nil)
             milter->mta (queue)
             mta->milter (queue)]
        (let [k (first keys)]
          (cond
            (.isReadable k) (case (.channel k)
                              mta-channel (let [[command data] (receive-packet mta-channel)]
                                            (println "MTA    ->" command data)
                                            (recur (rest keys)
                                                   milter->mta
                                                   (conj mta->milter [command data])))
                              milter-channel (let [[command data] (receive-packet milter-channel)]
                                               (println "milter ->" command data)
                                               (recur (rest keys)
                                                      (conj milter->mta [command data])
                                                      mta->milter)))
            (.isWritable k) (case (.channel k)
                              mta-channel (unless (empty? milter->mta)
                                            (let [[command data] (peek milter->mta)]
                                              (println "MTA    <-" command data)
                                              (send-packet mta-channel command data)
                                              (recur (rest keys)
                                                     (pop milter->mta)
                                                     mta->milter)))
                              milter-channel (unless (empty? mta->milter)
                                               (let [[command data] (peek mta->milter)]
                                                 (println "milter <-" command data)
                                                 (send-packet milter-channel command data)
                                                 (recur (rest keys)
                                                        milter->mta
                                                        (pop mta->milter)))))))))))
