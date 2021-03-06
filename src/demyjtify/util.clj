(ns demyjtify.util
  (:require [clojure.java.io :as io]
            [onelog.core :as log]
            [bnb4clj.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmacro error [type & args]
  `(throw (ex-info "Protocol error" (hash-map :type '~type ~@args))))

(defmacro dprint [& args]
  `(log/debug+ ~@args))

(defn socket-reader [socket]
  #_(io/reader socket)
  (.getInputStream socket))

(defn socket-writer [socket]
  #_(io/writer socket)
  (.getOutputStream socket))

(defn reader [obj]
  (if (instance? java.net.Socket obj)
    (socket-reader obj)
    obj))

(defn writer [obj]
  (if (instance? java.net.Socket obj)
    (socket-writer obj)
    obj))

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

(defn bitmask->set
  "Return the list of keywords taken from MAP corresponding to the
  bitmask MASK."
  [mask map]
  (reduce-kv (fn [result key value]
               (if (zero? (bit-and value mask))
                 result
                 (conj result key)))
             #{} map))

(defn lazy-slurp [readable]
  (take-while pos? (repeatedly #(read-byte readable))))

(defmacro with-open-socket [[socket host port] & body]
  `(with-open [~socket (java.net.Socket. ~host ~port)]
     ~@body))

(defmacro with-connections [[socket port] & body]
  `(with-open [ss# (java.net.ServerSocket. ~port)]
     (for-ever
      (let [~socket (.accept ss#)]
        ~@body))))
