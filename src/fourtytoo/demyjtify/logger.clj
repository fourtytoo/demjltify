(ns demyjtify.logger
  (:import org.apache.log4j.AppenderSkeleton)
  ;; specify the Java class name to generate and what it extends:
  (:gen-class :name demyjtify.Logger
              :extends org.apache.log4j.AppenderSkeleton))

;; implement void append(LoggingEvent event):
(defn -append [_ event] ; first argument is 'this', ignored
  (println (.getMessage event))) ; just print the event message (to *out*)

;; implement void close()
(defn -close [_]) ; no resources to clean up

;; implement boolean requiresLayout()
(defn -requiresLayout [_] false) ; we handle all formatting
