(ns composition-kit.physical-sequence)

;; This is a set of functions which allow you to build a sequence of functions then 'play' them (have them triggered at their time)
;; once built. It's not really intended for adding events on live but I suppose you could do that, since it would work.
;;
;; The implementation is done by making the sequence 'object' an agent with a sorted set to hold the notes, sorted by time of
;; the item. We may want to sort by time then message type later. Lets see.


(defn ^:private compare-by-key-then [k]
  (fn [c1 c2]
    (let [comp  (compare (k c1) (k c2))]
      (if (zero? comp)
        (reduce (fn [s n] (if (zero? s) n s) )
                (map (fn [[k v]]
                       (if (and (instance? Comparable (first v)) (instance? Comparable (second v)))
                         (compare (first v) (second v))
                         (compare (hash (first v)) (hash (second v)))))
                     (merge-with (fn [& v] v) c1 c2))
                )
        comp)))
  )


(defn new-sequence []
  "Generate a new sequence object to which sequencable items can be added"
  {:seq (sorted-set-by (compare-by-key-then :time))
   :return-values []
   :play true })


(defn add-to-sequence [s & items]
  (let [item-maps (map (fn [ i t ] { :item i :time t }) (take-nth 2 items) (take-nth 2 (rest items)))
        new-set   (reduce conj (:seq s) item-maps)]
    (assoc s :seq new-set)))



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

      (<= (:time curr) rnow)   (let [return-value ((:item curr) rnow)
                                     return-values (conj (:return-values agent-data) return-value)
                                     ]
                                 (send *agent* play-on-thread t0-in-millis)
                                 (-> agent-data
                                     (assoc :seq (rest to-be-played))
                                     (assoc :return-values return-values)))

      :else                    (let [time-until  (- (:time curr) rnow)]
                                 ;; spin (with a little backoff sleeping)
                                 (send *agent* play-on-thread t0-in-millis)
                                 (when (> time-until 5) (Thread/sleep (* time-until 0.7)))
                                 agent-data)
      )
    )
  )


(defn play [ s & items ]
  (let [args  (apply hash-map items)         ]
    (send (agent s) play-on-thread (System/currentTimeMillis))
    )
  )

(defn stop [ s ] (send s (fn [agent-data] (assoc agent-data :play false))))

