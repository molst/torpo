(ns torpo.log)

(defn event "takes an event description and optionally a map containing additional information about the event" [description & more]
  (merge
    {:description description
     :date-of-occurrence (.getTime (java.util.Date.))}
    (first more)))

(defn log "takes either a string description of an event or an event map and logs it to file" [event-arg]
  (cond
    (string? event-arg) (spit (str "log.txt") (str (java.util.Date.) " ## " event-arg \newline) :append true)
    (map? event-arg)    (spit (str "log.txt") (str (java.util.Date. (:date-of-occurrence event-arg)) " ## "
                                                   (:node-of-occurrence event-arg) " ## "
                                                   (if-let [aspects (:aspects event-arg)] (str aspects " ## "))
                                                   (:description event-arg) \newline)
                              :append true)))
