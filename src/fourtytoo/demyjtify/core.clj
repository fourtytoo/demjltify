(ns fourtytoo.demyjtify.core
  (:require [clojure.tools.logging :as log]
            [clojure.tools.trace :as trace]
            [clojure.java.io :as io]
            [clj-logging-config.log4j :as logconf]
            [clj-time.core :as time]
            [clj-time.coerce :as tc]
            [clojure.string :as str])
  (:use [fourtytoo.bnb4clj]
        [fourtytoo.demyjtify.util]
        [fourtytoo.demyjtify.events]
        [fourtytoo.demyjtify.actions]))

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
    (enable-tracing)
    (setup-logging)
    (log/info "Strating server on port" port)
    ;; don't hang waiting the milter to return so that we still have the REPL
    (future (start-milter port identity))))