(ns composition-kit.compositions.scratch-pad
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))


;; Just a place for trying out syntax and APIS and what not.

(def instruments
  (-> (midi/midi-instrument-map)
      (midi/add-midi-instrument :inst-a (midi/midi-port 0))
      (midi/add-midi-instrument :inst-b (midi/midi-port 1))
      ))

(defn on-inst [s i] (ls/on-instrument s (i instruments)))
(def clock (tempo/constant-tempo 4 4 120))

(def scale
  (lily "^inst=inst-a ^hold=0.05 c*20 d*90  ^inst=inst-b e8*70 f ^hold=0.99 f4 g*120" :relative :c5 ))

(->
 (>>> 
  (-> scale ))
 (ls/with-clock clock)
 (midi-play)
 )


