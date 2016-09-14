(ns composition-kit.logical-sequence)

;; so we start with an item which can be sequenced, defined by a pair of multimethods
(defmulti music-item?    (fn [it] (:itemtype it)))
(defmethod music-item? :default [it] false)

(defmulti item-payload  (fn [it] (:itemtype it)))
(defmethod item-payload :default [it] (if (music-item? it) ( :payload it ) nil ))

(defmulti item-beat   (fn [it] (:itemtype it)))
(defmethod item-beat :default [it] (if (music-item? it) ( :beat it ) nil ))

;; We define two types of items; one which has a duration (like a note) and one which is simply at a point
;; in beat (like, say, a dynamics)

(defn notes-with-duration [ notes dur beat ]
  {:itemtype :notes-with-duration
   :payload  { :notes notes :dur dur }
   :beat     beat
   })
(defmethod music-item? :notes-with-duration [it] true)

(defn music-event [ event beat ]
  {:itemtype :music-event
   :payload  event
   :beat     beat
   })
(defmethod music-item? :music-event [it] true)

;; so a logical sequence is simply an object respodning to first and rest where the guarantee is that
;; (item-beat first) < (item-beat (first rest)) always. There are lots of ways to make these. Here are a few,
;; but with lazy-seq you can of course do more

(defn concrete-logical-sequence [items]
  (sort-by item-beat items))

;; Merged sequences are a lazy sequence which makes the earliest one its first. This is actually 
(defn merged-logical-sequences [sequences]
  (if (empty? sequences)
    []

    (let [first-els      (remove nil? (map first sequences))
          earliest-beat  (apply min (map item-beat first-els))
          headel-picked
          (loop [seqs    sequences
                 res     { :headel nil :rest [] }]
            (cond
              (empty? seqs) res
              
              (= earliest-beat (item-beat (first (first seqs))))
              (-> res
                  (assoc :headel (first (first seqs)))
                  (assoc :rest (concat (:rest res)
                                       (if (empty? (rest (first seqs))) [] [ (rest (first seqs)) ])
                                       (rest seqs))))
              
              :else (recur (rest seqs) (assoc res :rest (conj (:rest res) (first seqs)))))
            
            )]
      (lazy-seq (cons (:headel headel-picked) (merged-logical-sequences (:rest headel-picked))))
      )
    )
  )


(defn sequence-from-pitches-and-durations [ pitch-pattern duration-pattern & options-list ]
  (let [options     (apply hash-map options-list)
        length      (get options :length :legato)
        volume      (get options :volume 100)
        start-beat  (get options :start-beat 0)
        ]
    (loop [pitches      pitch-pattern
           durations    duration-pattern
           curr-beat    start-beat
           res          [] ]
      (if (empty? pitches) res
          (let [fp       (first pitches)
                this-dur (case length
                           :legato    (first durations)
                           :staccato  (/ (first durations) 10)
                           :else      (* 0.95 (first durations)))
                this-item (notes-with-duration fp this-dur curr-beat)]
            (recur (rest pitches) (rest durations) (+ curr-beat (first durations)) (conj res this-item)))))))
           
    


