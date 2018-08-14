(ns composition-kit.compositions.synth-quartets.sq-01-a
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core)
  )

(def clock (tempo/constant-tempo 4 4 120))

(def instruments
  (-> (midi/midi-instrument-map)
      (midi/add-midi-instrument :high-sharp (midi/midi-port 0))

      ))

(defn on-inst [s i] (ls/on-instrument s (i instruments)))

(def lead-synth
  (>>>
   (-> (lily "c4 c' g8 fis e4" :relative :c5)
       (ls/hold-for-pct 1.02)
       (ls/explicit-dynamics '(100 104 87 84 81)))
   )
  )

(def final-song
  (->
   lead-synth
   (on-inst :high-sharp)
   (ls/with-clock clock)
   )
  )


(def play-quartet true)
(def tdelay -1)
(def ag
  (when play-quartet
    (midi-play
     final-song
     :beat-clock clock
     :beat-zero tdelay ;;tdelay ;;(+ 50 tdelay)
     ))

  )

