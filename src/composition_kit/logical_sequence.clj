(ns composition-kit.logical-sequence)

;; so we start with an item which can be sequenced, defined by a pair of multimethods
(defmulti music-item?    :itemtype)
(defmethod music-item? :default [it] false)

(defmulti item-payload   :itemtype)
(defmethod item-payload :default [it] (when (music-item? it) ( :payload it )))

(defmulti item-beat      :itemtype)
(defmethod item-beat :default [it] (when (music-item? it) ( :beat it )))

;; We define two types of items; one which has a duration (like a note) and one which is simply at a point
;; in beat (like, say, a dynamics). The note with a duration has a separate dimension of time which is how long
;; it is held for. Since notes can slur, echo, be staccato, be detache and so on, this is a number which
;; when rendered would tell you how much to hold inside the duration.

(defn notes-with-duration
  ([ notes dur beat ] (notes-with-duration notes dur beat 1))
  ([ notes dur beat hold-for]
   {:itemtype ::notes-with-duration
    :payload  { :notes notes :dur dur :hold-for hold-for }
    :beat     beat
    })
  )

(defmethod music-item? ::notes-with-duration [it] true)

(defn rest-with-duration [ dur beat ]
  {:itemtype ::rest-with-duration
   :payload { :dur dur }
   :beat    beat
   })
(defmethod music-item? ::rest-with-duration [it] true)

(defn music-event [ event beat ]
  {:itemtype ::music-event
   :payload  event
   :beat     beat
   })
(defmethod music-item? ::music-event [it] true)

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
              
              (= earliest-beat (item-beat (ffirst seqs)))
              (-> res
                  (assoc :headel (ffirst seqs))
                  (assoc :rest (concat (:rest res)
                                       (if (empty? (rest (first seqs))) [] [ (rest (first seqs)) ])
                                       (rest seqs))))
              
              :else (recur (rest seqs) (update-in res [ :rest ] conj (first seqs))))
            
            )]
      (lazy-seq (cons (:headel headel-picked) (merged-logical-sequences (:rest headel-picked))))
      )
    )
  )


;; FIXME - deal with rests
(defn sequence-from-pitches-and-durations [ pitch-pattern duration-pattern & options-list ]
  (let [options     (apply hash-map options-list)
        length      (get options :length :standard)
        volume      (get options :volume 100)
        start-beat  (get options :start-beat 0)
        ]
    (loop [pitches      pitch-pattern
           durations    duration-pattern
           curr-beat    start-beat
           res          [] ]
      (if (empty? pitches) res
          (let [fp       (first pitches)
                this-hold (case length
                            :legato    1
                            :staccato  0.1
                            :standard  0.95)
                this-item (notes-with-duration fp (first durations) curr-beat this-hold)]
            (recur (rest pitches) (rest durations) (+ curr-beat (first durations)) (conj res this-item)))))))

(defn ^:private loop-sequence-helper [ original current-subset count offset phrase-length ]
  (if (empty? current-subset)
    (if (<= count 1)
      [] ;; we just finished playing something we want looped once; so we are done
      (loop-sequence-helper original original (dec count) (+ offset phrase-length) phrase-length))
    ;; So our current helper is not zero so:
    (let [fc  (first current-subset)
          fcb (item-beat fc)
          rb  (assoc fc :beat (+ fcb offset))]
      (lazy-seq (cons rb (loop-sequence-helper original (rest current-subset) count offset phrase-length ))))
    ))

(defn loop-sequence [ original count ]
  (loop-sequence-helper original original count 0
                        (reduce
                         +
                         (map
                          (comp :dur item-payload)
                          (filter (comp not nil? :dur item-payload) original)
                          )
                         )
                        )
  )

