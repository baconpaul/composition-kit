(ns composition-kit.logical-sequence)

;; shower idea
;; + itemtype is a function
;; + have a transformer-item with functions inserted for all the methods pointing at an underlyer
;; + test the transformer
;; - modify beat shift to use it
;; - modify loop to use it
;; + test item-type explicitly

;; so we start with an item which can be sequenced, defined by a set of multimethods
(defmulti music-item?    :itemtype)
(defmethod music-item? :default [it] false)

(defmulti item-payload   :itemtype)
(defmethod item-payload :default [it] (when (music-item? it) ( :payload-72632 it )))

(defmulti item-beat      :itemtype)
(defmethod item-beat :default [it] (when (music-item? it) ( :beat-2314 it )))

(defmulti item-end-beat   :itemtype)
(defmethod item-end-beat :default [it] (item-beat it))

(defmulti item-type    :itemtype)
(defmethod item-type :default [it] (when (music-item? it) (:itemtype it)))

(defmulti item-has-dynamics? :itemtype)
(defmethod item-has-dynamics? :default [it] nil)

(defmulti item-dynamics :itemtype)
(defmethod item-dynamics :default [it] nil)

;; We define two types of items; one which has a duration (like a note) and one which is simply at a point
;; in beat (like, say, a dynamics). The note with a duration has a separate dimension of time which is how long
;; it is held for. Since notes can slur, echo, be staccato, be detache and so on, this is a number which
;; when rendered would tell you how much to hold inside the duration.

(defn notes-with-duration
  ([ notes dur beat ] (notes-with-duration notes dur beat 1))
  ([ notes dur beat hold-for]
   {:itemtype ::notes-with-duration
    :payload-72632
    {
     :notes notes
     :dur dur
     :hold-for hold-for
     }
    :beat-2314     beat
    :dynamics-132  (constantly 80)
    })
  )

(defmethod music-item? ::notes-with-duration [it] true)
(defmethod item-end-beat ::notes-with-duration [it] (+ (:beat-2314 it) (:dur (:payload-72632 it))))
(defmethod item-has-dynamics? ::notes-with-duration [it] true)
(defmethod item-dynamics ::notes-with-duration [it] (:dynamics-132 it))

(defn rest-with-duration [ dur beat ]
  {:itemtype ::rest-with-duration
   :payload-72632 { :dur dur }
   :beat-2314    beat
   })
(defmethod music-item? ::rest-with-duration [it] true)
(defmethod item-end-beat ::rest-with-duration [it] (+ (:beat-2314 it) (:dur (:payload-72632 it))))

(defn music-event [ event beat ]
  {:itemtype ::music-event
   :payload-72632  event
   :beat-2314     beat
   })
(defmethod music-item? ::music-event [it] true)

;; Each item of the transfomer gets handed the entire item.
(defn item-transformer [ underlyer payload-xform beat-xform end-beat-xform dynamics-xform ]
  {:itemtype ::item-transformer
   :underlyer  underlyer
   :payload-xform  (or payload-xform  item-payload)
   :beat-xform     (or beat-xform     item-beat)
   :end-beat-xform (or end-beat-xform item-end-beat)
   :dynamics-xform (or dynamics-xform item-dynamics)
   })
(defmethod music-item? ::item-transformer [it] true);
(defmethod item-beat ::item-transformer [it] ((:beat-xform it) (:underlyer it)))
(defmethod item-end-beat ::item-transformer [it] ((:end-beat-xform it) (:underlyer it)))
(defmethod item-payload ::item-transformer [it] ((:payload-xform it) (:underlyer it)))
(defmethod item-type ::item-transformer [it] (item-type (:underlyer it)))
(defmethod item-has-dynamics? ::item-transformer [it] (item-has-dynamics? (:underlyer it)))
(defmethod item-dynamics ::item-transformer [it] ((:dynamics-xform it) (:underlyer it)))



;; Dynamics
(defn note-dynamics-to-7-bit-volume [item]
  (when (item-has-dynamics? item)
    ((item-dynamics item) item)))


;; so a logical sequence is simply an object respodning to first and rest where the guarantee is that
;; (item-beat first) < (item-beat (first rest)) always. There are lots of ways to make these. Here are a few,
;; but with lazy-seq you can of course do more

(defn concrete-logical-sequence [items]
  (sort-by item-beat items))

;; Merged sequences are a lazy sequence which makes the earliest one its first. This is actually 
(defn merge-sequences [ & sequences]
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
      (lazy-seq (cons (:headel headel-picked) (apply merge-sequences (:rest headel-picked))))
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
          (let [fp        (first pitches)
                this-hold (* (first durations)
                             (case length
                               :legato    1
                               :staccato  0.1
                               :standard  0.95))
                this-item (notes-with-duration fp (first durations) curr-beat this-hold)]
            (recur (rest pitches) (rest durations) (+ curr-beat (first durations)) (conj res this-item)))))))

;; Time manipulation and looping
(defn beat-length [ sequence ]
  (- (apply max (map item-end-beat sequence)) (item-beat (first sequence))))

(defn beat-length-from-zero [ sequence ]
  (apply max (map item-end-beat sequence)))

(defn item-beat-shift [ it shift ]
  (item-transformer it nil (comp (partial + shift) item-beat) (comp (partial + shift) item-end-beat) nil))

(defn beat-shift [ sequence shift ]
  (map #(item-beat-shift % shift) sequence))

(defn loop-sequence [ original count ]
  (mapcat #(beat-shift original (* % (beat-length original))) (range count)))


(defn concat-sequences [ & concat-these ]
  "string sequences on after another"
  (mapcat (fn [se of] (beat-shift se of)) concat-these  (reductions + 0 (map beat-length concat-these)))
  )
