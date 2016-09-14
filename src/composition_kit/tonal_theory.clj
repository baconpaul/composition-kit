(ns composition-kit.tonal-theory)

(def ^:priave notes-data
  ;;(sorted-set-by (map-comparator :octave :notebase :accidental)
  (let [accidental-names { -2 "ees" -1 "es" 0 "" 1 "is" 2 "iis" }
        offset-from-c    { :c 0 :d 2 :e 4 :f 5 :g 7 :a 9 :b 11 }
        notelist (for [notebase   [ :a :b :c :d :e :f :g ]
                       accidental [ -2 -1 0 1 2 ]
                       octave     (range 8)]
                   {
                    :note   (keyword (str (name notebase) (get accidental-names accidental) octave))
                    :pitch  (keyword (str (name notebase) (get accidental-names accidental)))
                    :octave octave
                    :notebase notebase
                    :accidental accidental
                    :midinote  (+ 60 ( * 12 (- octave 4)) (notebase offset-from-c) accidental)
                    }
                   )]
    (reduce (fn [m v] (assoc m (:note v) v)) {} notelist)
    ))

(def ^:private notes-data-by-midinote
  (loop [n                   (vals notes-data)
         res                 {}]
    (if (empty? n) res
        (let [currn (first n)
              ss  (get res (:midinote currn) [] )
              ]
          (recur (rest n) (assoc res (:midinote currn) (conj ss currn))))))
  )

(defn note-by-name [name]
  "(note-by-name :cis2) works just fine!"
  (get notes-data name))

(defn notes-by-midinote [num]
  "(first (notes-by-midinote 60)) for instance"
  (get notes-data-by-midinote num))
  


(defn interval-from-c [note]
  "Return a positive interval from c. So
    (interval-from-c :cis) is 1
    (interval-from-c :b) is 11"
  (let [[ tone & accidental ] (name note)
        accidental-values { "ees" -2 "es" -1 "" 0 "is" 1 "iis" 2 }
        offset-from-c    { :c 0 :d 2 :e 4 :f 5 :g 7 :a 9 :b 11 }
        ]
    (mod (+ (offset-from-c (keyword (str tone))) (get accidental-values (reduce str accidental))) 12)
    )
  )

(defn interval-between [n1 n2]
  "From n1 to the nearest n2 above it"
  (mod (- (interval-from-c n2) (interval-from-c n1)) 12))


(defn ^:private intervals-to-degrees [ intervals ]
  (reductions (fn [prior ivl]
                (cond
                  (= ivl 'WH) (+ 3 prior)
                  (= ivl 'W) (+ 2 prior)
                  (= ivl 'H) (inc prior)
                  )) 0 intervals))

(def ^:private scales-data
  (let  [mode-r
         (fn [n]
           (let [maj '( W W H W W W H )]
             (concat (drop n maj) (take n maj))))]
    {
     :major          (intervals-to-degrees '( W W H W W W H ))
     :natural-minor  (intervals-to-degrees '( W H W W H W W ))
     :harmonic-minor (intervals-to-degrees '( W H W W H WH H ))

     :ionian         (intervals-to-degrees (mode-r 0))
     :dorian         (intervals-to-degrees (mode-r 1))
     :phrygian       (intervals-to-degrees (mode-r 2))
     :lydian         (intervals-to-degrees (mode-r 3))
     :mixolydian     (intervals-to-degrees (mode-r 4))
     :aeolian        (intervals-to-degrees (mode-r 5))
     :locrian        (intervals-to-degrees (mode-r 6))
     
     }
    )
  )

(defn scale [type] (type scales-data))
(defn known-scales [] (keys scales-data))

(defn scale-to-notes [ in-scale base ]
  ;; A naive implementation, (map #(en-choice (get notes-by-midinote
  ;; (+ % (:midinote base)))) scale))) works fine but has all sorts of
  ;; problems with not picking the right note. So you need to do some enharmonic
  ;; optimization (I guess it woudl be called), hence the explicit loop here where
  ;; we pick the one with notename one up from prior, unless we can't, in which case
  ;; we just punt
  (loop [scale      (rest in-scale)
         res        [ base ]]
    (if (empty? scale) res
        (let [prior           (last res)
              next-mn         (+ (:midinote base) (first scale))
              next-note-cands (notes-by-midinote next-mn)
              ;; The magic happens right here when we pick the next notebase keyword in string math space
              next-notebase   (-> (name (:notebase prior))
                                  (#(char (inc (int (first %)))))
                                  (#(if (= % \h) \a %))
                                  str
                                  keyword)
              filtered        (filter
                               #(= (:notebase %) next-notebase)
                               next-note-cands)
              best            (if (empty? filtered)
                                (first next-note-cands) ;; PUNT!
                                (first filtered))]
          (recur (rest scale) (conj res best))
          )))
  )











