(ns composition-kit.tempo)

;; We have a few key operations we can do on a tempo clock. Define them as multimethods switched on :clocktype
(defmulti metronome?           :clocktype)
(defmethod metronome? :default [cl] false)

(defmulti beats-to-time        (fn [cl b] (:clocktype cl))) ;; beats since epoch. 0
(defmulti measure-beat-to-beat (fn [cl m b] (:clocktype cl))) ;; beats in measure. 1 2 3 4
(defmulti measure-beat-to-time (fn [cl m b] (:clocktype cl)))


;; Start with the simplest constant tempo clock and expand later using multimethods to get the times and so on
(defn constant-tempo [ beats-per-measure unit-of-measure bpm ]
  {:clocktype ::constant
   :bpm       bpm
   :spb       (/ 60 bpm)
   :beats-per-measure beats-per-measure
   :unit-of-measure   unit-of-measure
   })

(defmethod metronome?           ::constant [clock] true)
(defmethod beats-to-time        ::constant [clock beats] ( * beats (:spb clock)) )
(defmethod measure-beat-to-beat ::constant [clock measure beat] (+ (dec beat) ( * (dec measure) (:beats-per-measure clock))))
(defmethod measure-beat-to-time ::constant [clock measure beat] (beats-to-time clock (measure-beat-to-beat clock measure beat)))

(defn multi-segment-constant-tempi [ beats-per-measure unit-of-measure & measures-and-tempi ]
  "This makes a clock which has constant tempo for a period then, at the start of a measure,
switches to a new tempo. For instance here is a 7/8 which speeds up at measure 10 and slows down
at measure 12

   (multi-segment-constant-tempi 7 8
       0   120
       10  160
       12  104)"
  (let [measures (take-nth 2 measures-and-tempi)
        tempi    (take-nth 2 (rest measures-and-tempi))
        beats-at-transition (map #(* (dec %) beats-per-measure) measures)
        spb      (map #(/ 60 %) tempi)
        ]
    {:clocktype ::multi-segment-constant
     :beats-per-measure beats-per-measure
     :unit-of-measure unit-of-measure

     :tempo-data (map (fn [a b c d] { :measure a :tempo b :spb c :beats-at-transition d })
                      measures tempi spb beats-at-transition )
     })
  )

(defmethod metronome?          ::multi-segment-constant [cl] true)

;; Remember this is still a constant signature metronome so the measure-beat-to-beat is the same. The variability
;; comes in the beat-to-time implementation
(defmethod measure-beat-to-beat ::multi-segment-constant [clock measure beat] (+ (dec beat) ( * (dec measure) (:beats-per-measure clock))))
(defmethod measure-beat-to-time ::multi-segment-constant [clock measure beat] (beats-to-time clock (measure-beat-to-beat clock measure beat)))
(defmethod beats-to-time ::multi-segment-constant [clock beat]
  (let [tempos  (take-while #(<= (:beats-at-transition %) beat) (:tempo-data clock))
        ltemp  (last tempos)
        extra-beats (- beat (:beats-at-transition ltemp))
        extra-time  (* (:spb ltemp) extra-beats)
        beats-at-sbp (map (fn [a b] (* (:spb a) (- ( :beats-at-transition b) (:beats-at-transition a))))
                          tempos (rest tempos))
        prior-time  (reduce + beats-at-sbp)
        ]
    (+ prior-time extra-time)
    )
  )

