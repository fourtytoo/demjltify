(ns demyjtify.sample
  (:require [clojure.test :refer :all]
            [demyjtify.core :refer :all]))

(defonce message-counter (atom 0))
(defonce byte-counter (atom 0))

(defn print-totals [context]
  (println "Message" @message-counter "of" (context :byte-count) "bytes")
  (println "the messages seen so far total" @byte-counter "bytes")
  (println "for an average of" (/ @byte-counter @message-counter) "bytes per message"))

(def handlers
  {:body (fn [e c]
           (println "BODY" e)             ; -wcp25/1/15.
           (send-action {:action :continue} c)
           (update-in c [:byte-count]
                      #(+ % (count (e :data)))))
   :mail (fn [e c]
           (println "MAIL" e)             ; -wcp25/1/15.
           (send-action {:action :continue} c)
           (assoc c :byte-count 0))
   :abort (fn [e c]
            (println "ABORT" e)             ; -wcp25/1/15.
            (->> (assoc c :byte-count 0)
                 (default-event-handler e)))
   :end-of-message (fn [e c]
                     (swap! byte-counter
                            #(+ % (c :byte-count)))
                     (swap! message-counter inc)
                     (print-totals c)
                     (default-event-handler e c))})

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
                          (assoc :events #{:mail :body})
                          (assoc :byte-count 0)
                          (assoc :handlers handlers)))))))
