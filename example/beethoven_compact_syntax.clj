(ns beethoven-compact
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])

  (use composition-kit.core))

;; The first few measures of op 13 mov 2 in a compact syntax

(def piano (midi/midi-instrument 1))
(def clock (tempo/constant-tempo 2 4 47))

(defn sixteenths [p1 p2 p3 p4]
  (->
   (ls/sequence-from-pitches-and-durations
    [p1 p2 p3 p4]
    (repeat 4 1/4))
   (ls/explicit-segment-dynamics '(67 73 64 69))))

(defn alternates [p1 p2] (sixteenths p1 p2 p1 p2))



(def beethoven-phrase
  (<*>
   (->
    (lily :relative :c4 "c4 bes ees4. des8 c ees aes bes ees,4. e8")
    (ls/line-segment-dynamics 0  90
                              4  85
                              11/1  120
                              6  85
                              8  88))
   (>>>
    (alternates :aes3 :ees3)
    (alternates :g3 :ees3)
    (alternates :aes3 :ees3)
    (alternates :bes3 :ees3)
    (sixteenths :aes3 :ees3 :bes3 :ees3)
    (sixteenths :c4 :aes3 :d4 :aes3)
    (alternates :g3 :bes3)
    (alternates :g3 :bes3))
   (->
    (lily  "aes4 des c g aes8 g f f' ees4 ees," :relative :c3)
    (ls/line-segment-dynamics 0 80
                              4 75
                              8 90))
   ;; could also do a (control 64 here here with the actual times but pedal is shorthand
   (ls/pedal-held-and-cleared-at 0 1 2 4 5 6 8)
   )
  )


(def song
  (-> beethoven-phrase
      (ls/on-instrument piano)
      (ls/with-clock clock)))


(midi-play song)




