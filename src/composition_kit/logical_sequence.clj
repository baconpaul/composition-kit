(ns composition-kit.logical-sequence)

;; so we start with an item which can be sequenced, defined by a pair of multimethods
(defmulti music-item?    (fn [it] (:itemtype it)))
(defmethod music-item? :default [it] false)

(defmulti item-payload  (fn [it] (:itemtype it)))
(defmethod item-payload :default [it] (if (music-item? it) ( :payload it ) nil ))

(defmulti item-time   (fn [it] (:itemtype it)))
(defmethod item-time :default [it] (if (music-item? it) ( :time it ) nil ))

;; We define two types of items; one which has a duration (like a note) and one which is simply at a point
;; in time (like, say, a dynamics)

(defn notes-with-duration [ notes dur time ]
  {:itemtype :notes-with-duration
   :payload  { :notes notes :dur dur }
   :time     time
   })
(defmethod music-item? :notes-with-duration [it] true)

(defn music-event [ event time ]
  {:itemtype :music-event
   :payload  event
   :time     time
   })
(defmethod music-item? :music-event [it] true)

;; Next we need an instrument abstraction. An instrument is a very simple thing on which we can play an
;; event instantly. That's all of the contract. We don't actually define any instruments here, just the
;; methods
(defmulti instrument? (fn [it] (:instrument-type it)))
(defmulti play-on (fn [instrument musicitem] [ (:instrument-type instrument) (:itemtype musicitem) ] ))

;; Now we define an instrument-bound item which contains a subordinate item
(defn instument-bound-item [ instrument item ]
  {:itemtype :instrument-bound
   :payload (fn [t] (play-on instrument item))
   :time    (:time item)
   })
(defmethod music-item? :instrument-bound [it] true)


;; so a logical sequence is simply an object respodning to first and rest where the guarantee is that
;; (item-time first) < (item-time (first rest)) always. There are lots of ways to make these. Here are a few,
;; but with lazy-seq you can of course do more

(defn concrete-logical-sequence [items]
  (sort-by item-time items))

;; Merged sequences are a lazy sequence which makes the earliest one its first. This is actually 
(defn merged-logical-sequences [sequences]
  (if (empty? sequences)
    []

    (let [first-els      (remove nil? (map first sequences))
          earliest-time  (apply min (map item-time first-els))
          headel-picked
          (loop [seqs    sequences
                 res     { :headel nil :rest [] }]
            (cond
              (empty? seqs) res
              
              (= earliest-time (item-time (first (first seqs))))
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





