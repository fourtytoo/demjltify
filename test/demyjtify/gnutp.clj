(ns fourtytoo.demyjtify.gnutp
  (:require [clojure.test :refer :all])
  (:use [fourtytoo.demyjtify.core]
        [fourtytoo.demyjtify.actions :refer (send-action)]
        [fourtytoo.demyjtify.events :refer (default-event-handler)]))

;;; Dedicated to the memory of Sir Terence David John Pratchett

(def the-header "X-Clacks-Overhead")

(define-event-handlers handlers
  (:header
   (default-event-handler event
     (if (.equalsIgnoreCase the-header (event :name))
       (assoc context :clacks-overhead true)
       context)))
  (:mail
   (send-action {:action :continue} context)
   (assoc context :clacks-overhead false))
  (:abort
   (->> (assoc context :clacks-overhead false)
        (default-event-handler event)))
  (:end-of-message
   (when (not (context :clacks-overhead))
     (send-action {:action :add-header
                   :name the-header
                   :value "GNU Terry Pratchett"}
                  context))
   (default-event-handler event context)))

(defn run-sample [& args]
  (start-milter 12315
                (fn [ctx]
                  (println "got MTA connection" ctx)
                  (-> ctx
                      (assoc :handlers handlers)
                      (assoc :actions #{:add-header})))))
