(ns composition-kit.compositions.sketches.fine-orchestra-control
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core)
  )

(def instruments
  (-> (midi/midi-instrument-map)
      (midi/add-midi-instrument :ep (midi/midi-port 0))
      ))

(def clock (tempo/constant-tempo 4 4 132))
(defn on-inst [s i] (ls/on-instrument s (i instruments)))

(def mary-theme
  (lily "e4 d c d <e g> <e g> <e g>2 d4 d d2 e4 e e2"
        :relative :c5))

(-> mary-theme
    (ls/with-clock clock)
    (on-inst :ep)
    (midi-play))
