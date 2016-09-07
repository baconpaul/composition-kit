(ns composition-kit.music-sequence
  (import (java.util.concurrent Executors)))

(defn ^:private compare-by-key-then [k]
  (fn [c1 c2]
    (let [comp  (compare (k c1) (k c2))]
      (if (zero? comp)
        (compare c1 c2)
        comp)))
  )

(defn new-sequence [] (atom (sorted-set-by (compare-by-key-then :time))))

(defn ->sequence [s & items]
  (loop [ss              s
         [ i t & rest]   items ]
    (if (nil? i) s
        (recur (swap! s conj { :item i :time t }) rest)))
  )

(defn ^:private play-on-thread [agent-data t0-in-millis]
  ;; So basically we will do a spin loop here playing and stripping the sequence
  (loop [to-be-played      @(:seq agent-data)
         ]
    (let [rnow        (- (System/currentTimeMillis) t0-in-millis)
          curr        (first to-be-played)
          ]
      (if (nil? curr) (assoc agent-data :play false)
          (if (<= (:time curr) rnow) (do ((:item curr) rnow) (recur (rest to-be-played)))
              (let [time-until  (- (:time curr) rnow)]
                ;; Basically don't spin if I know i have to wait
                (when (> time-until 2) (Thread/sleep (* time-until 0.7)))
                (recur to-be-played))))))
  )

(defn play [ s & items ]
  (let [args  (apply hash-map items)
        play-agent  (agent {:seq s :play true})
        ]
    (send play-agent play-on-thread (System/currentTimeMillis))
    )
  )
