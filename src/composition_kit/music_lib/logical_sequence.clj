(ns composition-kit.music-lib.logical-sequence
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:use [composition-kit.music-lib.logical-item])
  )

;; so a logical sequence is simply an object respodning to first and rest where the guarantee is that
;; (item-beat first) < (item-beat (first rest)) always. There are lots of ways to make these. Here are a few,
;; but with lazy-seq you can of course do more

(defn concrete-logical-sequence [items]
  (sort-by item-beat items))

;; Merged sequences are a lazy sequence which makes the earliest one its first. This is actually 
(defn merge-sequences [ & in-sequences]
  (let [sequences      (filter (comp not empty?) in-sequences)]
    (if (empty? sequences)
      []
      
      (let [
            first-els      (remove nil? (map first sequences))
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
                this-item (if (nil? fp)
                            (rest-with-duration (first durations) curr-beat)
                            (notes-with-duration fp (first durations) curr-beat this-hold))]
            (recur (rest pitches) (rest durations) (+ curr-beat (first durations)) (conj res this-item)))))))

(def explicit-phrase sequence-from-pitches-and-durations)

(defn sequence-from-pitches-constant-duration [ pitch-pattern duration & options-list ]
  (apply sequence-from-pitches-and-durations (concat [pitch-pattern
                                                      (map (constantly duration) pitch-pattern)]
                                                     options-list)))

(defn repeated-note [ note duration repeats & options ]
  (apply sequence-from-pitches-and-durations (concat [(repeat repeats note)
                                                      (repeat repeats duration)]
                                                     options)))


;; Time manipulation and looping
(defn beat-length [ sequence ]
  (if (empty? sequence) 0
      (- (apply max (map item-end-beat sequence)) (item-beat (first sequence)))))

(defn beat-length-from-zero [ sequence ]
  (if (empty? sequence) 0
      (apply max (map item-end-beat sequence))))

(defn beat-shift [ sequence shift ]
  (map #(item-beat-shift % shift) sequence))

(defn loop-sequence [ original count ]
  (mapcat #(beat-shift original (* % (beat-length original))) (range count)))

;; An alias
(def loop-n loop-sequence)

(defn concat-sequences [ & concat-these ]
  "string sequences on after another"
  (mapcat (fn [se of] (beat-shift se of)) concat-these  (reductions + 0 (map beat-length concat-these)))
  )

(defn transform-when [sequence pred xform value]
  (map #(if (pred %)
          (-> (identity-item-transformer %)
              (add-transform xform value))
          %) sequence))

(defn transform-note-payload [sequence value]
  (transform-when sequence is-notes-with-duration? :payload
                  #(let [p (item-payload %)] (value % p))))

(defn transform [sequence xform value]
  (map #(-> (identity-item-transformer %)
            (add-transform xform value)) sequence))

(defn transpose [sequence amt]
  (transform-note-payload
   sequence
   (fn [i p]
     (let [no   (:notes p)
           ns   (if (seq? no) no [no])
           nns  (map #(-> %
                          (th/note-by-name)
                          (th/transpose amt)
                          :note) ns)]
       (assoc p :notes nns)))))

(defn overlay-transpose [sequence amt]
  (merge-sequences sequence (transpose sequence amt)))

(defn overlay-octave-below [sequence]
  (merge-sequences sequence (transpose sequence -12)))

(defn with-clock [ sequence clock ]
  (transform sequence :clock (constantly clock)))

(defn on-instrument [ sequence instrument ]
  (transform sequence :instrument (constantly instrument)))


;; Dynamics applicators
(defn override-sequence-dynamics [ mseq dfn ]
  "Replace the dynamics function of every element of the sequence with dfn"
  (map #(override-dynamics % dfn) mseq))

(defn amplify [ mseq pct ]
  (map #(amplify-by % pct) mseq))

(defn hold-for [mseq beats]
  (transform-note-payload mseq (fn [i p] (assoc p :hold-for beats))))

(defn hold-for-pct [mseq pct]
  (transform-note-payload mseq (fn [i p] (assoc p :hold-for (* (:dur p) pct)))))

(defn explicit-dynamics [mseq & levs]
  "Set the dynamics for each note. If you don't supply enough dynamics the last one
repeats for the remainder of the sequence"
  (let [values    (if (seq? (first levs)) (first levs) levs)
        usevalues (if (> (count mseq) (count values))
                    (concat values (repeat (last values)))
                    values)]
    (map (fn [n d] (override-dynamics n (constantly d))) mseq usevalues)))

;; deprecated name
(def explicit-segment-dynamics explicit-dynamics)

(defn line-segment-dynamics [ series & dynamics ]
  "Given a line segment set of beat / level pairs, set the dynamics accordingly.
Notes in the sequence before the first or after the last beat get the flat value of the
first or last beat. So

  (line-segment-dynamics notes 0 10 4 20 8 127)

is a slow then fast crescendo"
  (let [beats-n-levels  (partition 2 dynamics)
        vol-fn   (fn [item]
                   (let [beat  (item-beat item)
                         prior (or (last (take-while #(<= (first %) beat) beats-n-levels)) (first beats-n-levels))
                         nxt   (or (first (drop-while #(<= (first %) beat) beats-n-levels)) (last beats-n-levels))
                         
                         frac  (if (= prior nxt) 1 (/ (- beat (first prior)) (- (first nxt) (first prior))))
                         frac  (max (min frac 1) 0)
                         val   (+ (* frac (second nxt)) (* (- 1 frac) (second prior)))
                         ]
                     (int val)))
        ;; We have to "render" this now since item beats may change later
        vol-vals (map vol-fn series)
        ]
    (explicit-segment-dynamics series vol-vals)))


(defn line-segment-amplify [ series & dynamics ]
  "Given a line segment set of beat / amp pairs, set the dynamics accordingly.
Notes in the sequence before the first or after the last beat get the flat value of the
first or last beat. So

  (line-segment-amplify notes 0 0.9 20 1.1)

is a slow then fast crescendo on top of the undelrying dynmics"
  (let [beats-n-levels  (partition 2 dynamics)
        vol-fn   (fn [item]
                   (let [beat  (item-beat item)
                         prior (or (last (take-while #(<= (first %) beat) beats-n-levels)) (first beats-n-levels))
                         nxt  (or (first (drop-while #(<= (first %) beat) beats-n-levels)) (last beats-n-levels))
                         
                         frac  (if (= prior nxt) 1 (/ (- beat (first prior)) (- (first nxt) (first prior))))
                         under (or (note-dynamics-to-7-bit-volume item) 0)
                         val   (min 127 (* (+ (* frac (second nxt)) (* (- 1 frac) (second prior)))
                                           under))
                         ]
                     (int val)))
        ;; We have to "render" this now since item beats may change later
        vol-vals (map vol-fn series)
        ]
    (explicit-segment-dynamics series vol-vals)))

(defn pedal-held-and-cleared-at [ & arguments ]
  "Given a collection of beats, depress the pedal just a smidge after the beat and then
hold it until the next beat, where it releases and re-applies. So basically pedal clears are
at each of the arguments. The last argument ends the pedal."
  (let [shiftarg  (concat (rest arguments) [(last arguments)])
        fromto    (map (fn [ a b ] (list a b)) arguments shiftarg)

        ramps     (mapcat (fn [[s e]]
                            (if (= s e) ;; we are at the end so just turn off
                              [ [ s 0 ] ]
                              [ [ (+ s 0.02) 127 ] [ e 0 ] ])) fromto )

        result    (concrete-logical-sequence (map (fn [[b l]] (sustain-pedal-event l b)) ramps))
        ]
    result
    ))
