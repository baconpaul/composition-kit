(ns composition-kit.events.physical-sequence
  (:require [composition-kit.events.transport-window :as tw])
  )

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
   :play true
   :transport nil })


(defn add-to-sequence [s & items]
  (if (not (zero? (mod (count items) 2)))
    (throw (ex-info "Unable to find message/time pairs" {:items items}))
    (let [item-maps (map (fn [ i t ] { :item i :time t }) (take-nth 2 items) (take-nth 2 (rest items)))
          new-set   (reduce conj (:seq s) item-maps)]
      (assoc s :seq new-set))))



(defn ^:private play-on-thread [agent-data t0-in-millis]
  ;; this function schedules the next event and gets a little spinny as the event comes near so we make
  ;; sure we hit the milisecond accuracy. It also allows you to stop the playing as you go
  (let [rnow              (- (System/currentTimeMillis) t0-in-millis)
        to-be-played      (:seq agent-data)
        curr              (take-while #(<= (:time %) rnow) to-be-played)
        future-item       (drop-while #(<= (:time %) rnow) to-be-played)
        t-w               (:transport agent-data)
        ]
    (when (and t-w (:assoc t-w)) ((:assoc t-w ) :time rnow))
    (cond
      (empty? to-be-played)    (do
                                 (when t-w ((:close t-w)))
                                 (assoc agent-data :play false)
                                 )

      (not (:play agent-data)) agent-data  ;; someone stopped me in another action so just chillax

      (not (empty? curr)   )   (do
                                 (send *agent* play-on-thread t0-in-millis)
                                 (reduce
                                  (fn [data curr-el] ;; evaluate the item and glie it onto the return values
                                    (let [return-value ((:item curr-el) rnow)
                                          return-values (conj (:return-values data) return-value)
                                          ]
                                      (-> data
                                          (assoc :return-values return-values))))
                                  (assoc agent-data :seq future-item) ;; onto agent data which has rest attached
                                  curr) ;; for each one we stripped off
                                 )

      :else                    (let [time-until  (- (:time (first to-be-played)) rnow)]
                                 ;; spin (with a little backoff sleeping)
                                 (send *agent* play-on-thread t0-in-millis)
                                 (when (> time-until 5) (Thread/sleep (min 100 (* time-until 0.7)))) ;; that 100 keeps clock tickin
                                 agent-data)
      )
    )
  )


(defn stop [ s ] (send s (fn [agent-data] (assoc agent-data :play false))))

(defn play [ s & items ]
  (let [args  (apply hash-map items)
        use-transport true
        mod-s
        (if use-transport
          (let [t-w (tw/make-transport-window "Transport")]
            (assoc s :transport t-w))
          s)
        ag   (agent mod-s)
        ]
    (when use-transport
      ((:on-stop (:transport @ag))
       (fn []
         (do
           (stop ag)
           (when-let [us (:user-stop args)]
             (send ag (fn [d] (us) d))))
         )
       ))
    
    (send ag play-on-thread (System/currentTimeMillis))
    )
  )


