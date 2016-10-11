(ns beethoven-compact
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])

  (use composition-kit.core))

;; The first few measures of op 13 mov 2 in a compact syntax

(def piano (midi/midi-instrument 0))
(def clock (tempo/constant-tempo 2 4 47))

(defn sixteenths [p1 p2 p3 p4]
  (phrase
   (pitches   p1 p2 p3 p4)
   (apply durations (repeat 4 1/4))
   (dynamics  70 67 69 64)))

(defn alternates [p1 p2] (sixteenths p1 p2 p1 p2))



(def beethoven-phrase
  (overlay
   (phrase
    (lily :relative :c4 "c4 bes ees4. des8 c ees aes bes ees,4. e8")
    (dynamics-at 0 -> 90
                 4 -> 85
                 11/1 -> 93
                 6 -> 85
                 8 -> 88))
   (concatenate
    (alternates :aes3 :ees3)
    (alternates :g3 :ees3)
    (alternates :aes3 :ees3)
    (alternates :bes3 :ees3)
    (sixteenths :aes3 :ees3 :bes3 :ees3)
    (sixteenths :c4 :aes3 :d4 :aes3)
    (alternates :g3 :bes3)
    (alternates :g3 :bes3))
   (phrase
    (lily :relative :c3 "aes4 des c g, aes8 g f f' ees4 ees,")
    (dynamics-at 0 -> 80
                 4 -> 75
                 8 -> 90))
   ;; could also do a (control 64 here here with the actual times but pedal is shorthand
   (pedal-held-and-cleared-at 0 1 2 4 5 6 8)
   )
  )


(midi-play (on-instrument (with-clock beethoven-phrase clock) piano))









