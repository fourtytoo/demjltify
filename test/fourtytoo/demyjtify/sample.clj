(ns fourtytoo.demyjtify.sample
  (:require [clojure.test :refer :all]
            [clj-time.core :as time]
            [clj-time.coerce :as tc]
            [clojure.tools.trace :as trace]
            [clojure.tools.logging :as log]
            [clj-logging-config.log4j :as logconf])
  (:use [fourtytoo.demyjtify.core]
        [fourtytoo.demyjtify.actions :refer (send-action)]
        [fourtytoo.demyjtify.events :refer (default-event-handler)]))

(defonce message-counter (atom 0))
(defonce byte-counter (atom 0))

(defn print-totals [context]
  (println "Message" @message-counter "of" (context :byte-count) "bytes")
  (println "the messages seen so far total" @byte-counter "bytes")
  (println "for an average of" (/ @byte-counter @message-counter) "bytes per message"))

(define-event-handlers handlers
  (:body
   (print "+")
   (send-action {:action :continue} context)
   (update-in context [:byte-count]
              #(+ % (count (event :data)))))
  (:mail
   (print "MAIL ")
   (send-action {:action :continue} context)
   (assoc context :byte-count 0))
  (:abort
   (println " *ABORT*")
   (->> (assoc context :byte-count 0)
        (default-event-handler event)))
  (:end-of-message
   (println " EOM")
   (swap! byte-counter
          #(+ % (context :byte-count)))
   (swap! message-counter inc)
   (print-totals context)
   (default-event-handler event context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn log-formatter [log]
  (println (str (tc/to-date (log :timeStamp)))
           (str "[" (log :threadName) "]")
           (log :message))
  (flush))

(def this-name-space *ns*)

(defn enable-tracing [namespace]
  (trace/trace-ns namespace)
  ;; no need to also trace the loggin function
  (trace/untrace-var* 'log-formatter))

(defn setup-logging []
  (logconf/set-logger! :name "console"
                       :level :debug
                       :append true
                       :out log-formatter))

(defn debug-milter [& args]
  (let [port (if (empty? args)
               4242
               (first args))]
    (enable-tracing this-name-space)
    (setup-logging)
    (log/info "Strating server on port" port)
    ;; don't hang waiting the milter to return so that we still have the REPL
    (future (start-milter port identity))))


(defn run-sample [& args]
  (let [port (if (empty? args)
               4242
               (first args))]
    (setup-logging)
    (println "Strating server on port" port)
    ;; don't hang waiting for the milter to return so that we don't
    ;; lock the REPL
    (future
      (start-milter port
                    (fn [ctx]
                      (println "got MTA connection" ctx)
                      (-> ctx
                          (assoc :byte-count 0)
                          (assoc :handlers handlers)))))))
