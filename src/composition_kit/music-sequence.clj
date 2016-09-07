(ns composition-kit.music-sequence
  (import (java.util.concurrent Executors)))

(defn ^:private compare-by-key-then [k]
  (fn [c1 c2]
    (let [comp  (compare (k c1) (k c2))]
      (if (zero? comp)
        (compare c1 c2)
        comp)))
  )

(defn new-sequence []
  (agent {:seq (sorted-set-by (compare-by-key-then :time))
          :play true }))


(defn ->sequence [s & items]
  (let [add-to-seq  (fn [agent-data item] (assoc agent-data :seq (conj (:seq agent-data) item)))]
    (last (map (fn [ i t ] (send s add-to-seq { :item i :time t } ))
               (take-nth 2 items)
               (take-nth 2 (rest items))
               )
          ))
  )


(defn ^:private play-on-thread [agent-data t0-in-millis]
  ;; this function schedules the next event and gets a little spinny as the event comes near so we make
  ;; sure we hit the milisecond accuracy. It also allows you to stop the playing as you go
  (let [rnow              (- (System/currentTimeMillis) t0-in-millis)
        to-be-played      (:seq agent-data)
        curr              (first to-be-played)
        ]
    (cond
      (nil? curr)              (assoc agent-data :play false)
      (not (:play agent-data)) agent-data  ;; someone stopped me in another action so just chillax
      (<= (:time curr) rnow)   (do
                                 ((:item curr) rnow)
                                 (send *agent* play-on-thread t0-in-millis)
                                 (assoc agent-data :seq (rest to-be-played)))
      :else                    (let [time-until  (- (:time curr) rnow)]
                                 (send *agent* play-on-thread t0-in-millis)
                                 (when (> time-until 2) (Thread/sleep (* time-until 0.7)))
                                 agent-data)
      )
    )
  )


(defn play [ s & items ]
  (let [args  (apply hash-map items)         ]
    (send s play-on-thread (System/currentTimeMillis))
    )
  )

(defn stop [ s ] (send s (fn [agent-data] (assoc agent-data :play false))))
  
