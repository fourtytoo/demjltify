(ns demyjtify.parser
  (:require [clojure.string :as str]
            [bnb4clj.core :refer :all]
            [demyjtify.util :refer :all]))

(defn decode-int [bytes]
  (reduce (fn [val byte] (bit-or (bit-shift-left val 8) byte)) 0 bytes))

(defn- decode-packet-data
  "Decode the packet BYTES according to its FORMAT-DESCRIPTION,
  splitting it in its subarts (fields).  FORMAT-DESCRIPTION is a
  list of keywords among:

  :CHAR		a character
  :C-STRING	a NULL terminated string (the null is stripped)
  :C-STRINGS	a sequence of :C-STRING
  :INT16	a two byte integer in NBO
  :INT32	a four byte integer in NBO"
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
          :c-strings (conj result (map bytes-to-string (partition-with zero? data)))
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



(defmulti parse-packet (fn [command data] command))

(defmethod parse-packet :default
  [command data]
  (error unknown-packet-type :command command :data data))

(defmethod parse-packet \A
  [command data]
  {:event :abort})

(defmethod parse-packet \B
  [command data]
  {:event :body :data data})

(defmethod parse-packet \C
  [command data]
  (with-packet-data data [host-name :c-string
                          family :char
                          port :int16
                          address :c-string]
    {:event :connect
     :host-name host-name :family family :port port :address address}))

(defmethod parse-packet \D
  [command data]
  (with-packet-data data [command :char
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
                                   namevals)))}))

(defmethod parse-packet \E
  [command data]
  {:event :end-of-message})

(defmethod parse-packet \H
  [command data]
  {:event :hello
   :greeting (first (decode-packet-data data '(:c-string)))})

(defmethod parse-packet \K
  [command data]
  {:event :disconnect})

(defmethod parse-packet \L
  [command data]
  (with-packet-data data [name :c-string
                          value :c-string]
    {:event :header
     :name name :value value}))

(defmethod parse-packet \M
  [command data]
  (with-packet-data data [sender :c-string
                          options :c-strings]
    {:event :mail
     :sender sender :options options}))

(defmethod parse-packet \N
  [command data]
  {:event :end-of-headers})

(defmethod parse-packet \O
  [command data]
  (with-packet-data data [version :int32
                          actions :int32
                          protocol-mask :int32]
    {:event :options
     :version version :actions actions :protocol-mask protocol-mask}))

(defmethod parse-packet \Q
  [command data]
  {:event :quit})

(defmethod parse-packet \R
  [command data]
  (with-packet-data data [address :c-string
                          options :c-strings]
    {:event :recipient
     :address address :options options}))

(defmethod parse-packet \T
  [command data]
  {:event :data})

(defmethod parse-packet \U
  [command data]
  {:event :unknown
   :command (first (decode-packet-data data '(:c-string)))})

