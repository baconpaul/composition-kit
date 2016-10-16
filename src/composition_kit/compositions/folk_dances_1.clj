(ns composition-kit.compositions.folk-dances-1
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])

  (use composition-kit.core))


(def piano (midi/midi-instrument 0))
(def clock (tempo/constant-tempo 1 8 120))


(def upperOne "
    < c  bes'>4. <c bes'>4
    << { bes'16 aes g8 f des ees } \\\\ {c4. c4 } >>

    <c bes'>4. <c bes'>4
    << { bes'16 aes g8 f des ees } \\\\ {c4. c4 } >>

    <f ees'>4. <f ees'>4
    << { ees'16 des c8 bes ges aes } \\\\ { f4. f4 } >>
    
    <f ees'>4. <f ees'>4
    << { ees'16 des c8 bes ges aes } \\\\ { f4. f4 } >>
  ")

(def play-it false)
(when play-it
  (-> (lily upperOne)
      (on-instrument piano)
      (with-clock clock)
      (midi-play)))
