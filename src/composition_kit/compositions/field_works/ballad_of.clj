(ns composition-kit.compositions.field-works.ballad-of
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))


;; TO DO
;;   Anything

(def instruments
  (-> (midi/midi-instrument-map)
      (midi/add-midi-instrument :piano (midi/midi-port 0))
      (midi/add-midi-instrument :synth-bass (midi/midi-port 1))
      (midi/add-midi-instrument :synth-flute-pad (midi/midi-port 2))
      ))

(defn on-inst [s i] (ls/on-instrument s (i instruments)))

(def clock (tempo/constant-tempo 6 8 110))


(def arpeggios
  {
   :first-major  (-> (lily "bes'16 c, ees bes' f c   bes'16 c, ees bes' f c   bes'16 c, ees bes' f c")
                     (ls/explicit-segment-dynamics (apply concat  (repeat 3 [ 87 73 75 85 71 68 ])))
                     )
   :first-minor  (->  (lily "g'16 bes, c g' ees bes    g'16 bes, c g' ees bes bes8 c8 g'")
                      (ls/explicit-segment-dynamics (apply concat  (repeat 5 [ 87 73 75 ])))
                      )
   :rise-section (->  (lily "bes'16 des, ees bes' ees, des     bes'16 des, ees bes' ees, des     bes'16 des, ees bes' ees, des 
  bes' ees, f bes f ees   bes' ees, f bes f ees   bes' ees, f bes f ees
  d' f, g d' g, f   d' f, g d' g, f   d' f, g d' g, f")
                      (ls/explicit-segment-dynamics (apply concat  (repeat 9 [ 87 73 75 85 71 68 ])))
                      (ls/line-segment-amplify 0 1.0 3 0.9 8 1.0)
                      )
   })




(def bass-accomp
  {
   :first-major (->  (lily "f4. d4. ees4." :relative :c3)
                     (ls/hold-for-pct 1.01))
   :rise-section (-> (lily "des4 ees8 f4 g8 a4 bes8 c2. ees4. d2. bes4." :relative :c3)
                     (ls/hold-for-pct 1.01)
                     )
   })

(def intro-pad
  (->
   (>>>
    [(i/notes-with-duration [ :g4 :ees5 ] 9 0)] 
    [(i/notes-with-duration [ :f4 :des5 ] 9 0 )]
    [(i/notes-with-duration [ :bes4 :d5 ] 4 0 )]
    )
   (ls/hold-for-pct 1.0)
   )
  )

(defn merge-for-tag [tag]
  (-> (<*>
       (-> (tag arpeggios) (on-inst :piano))
       (-> (tag bass-accomp) (on-inst :synth-bass))
       (-> (tag bass-accomp) (on-inst :piano)))
      (ls/with-clock clock)
      )
  )

(defn try-out [p i]
  (-> p (on-inst i) (ls/with-clock clock) (midi-play)))

;;(try-out (:rise-section arpeggios) :piano)

(->
 (>>>
  (->
   intro-pad
   (on-inst :synth-flute-pad)
   (ls/with-clock clock)
   )
  
  (<*>
   (->
    intro-pad
    (on-inst :synth-flute-pad)
    (ls/with-clock clock)
    )
   (>>>
    (->  (merge-for-tag :first-major)  (ls/loop-n 2))
    (merge-for-tag :rise-section)
    )
   )
  )
 (ls/loop-n 40)
 (midi-play :beat-zero -1 :beat-clock clock)
 first
 )








