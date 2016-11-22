(ns composition-kit.compositions.folk-dances.folk-dances-4
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

(def instruments
  (-> (midi/midi-instrument-map)
      (midi/add-midi-instrument :drums (midi/midi-port 0))
      ))

(def clock (tempo/constant-tempo 3 8 77))

(def dp  (->  (ls/explicit-phrase [ :c4 :c4 :c4 :d4 :c4 :c4] [ 1/2 1/2 1/2 1/2 1/2 1/2]) (ls/loop-n 4)) )

(def final-song
  (-> dp (ls/on-instrument (:drums instruments)) (ls/with-clock clock)))

(def ag  (midi-play
          final-song
          :samples [ {:file "/Users/paul/Desktop/MM/Bouncedown.wav"  :zero-point  (* (tempo/beats-to-time clock -3) 1000000)}]
          :beat-clock clock
          :beat-zero 4
          )

  )
(agent-errors ag)
