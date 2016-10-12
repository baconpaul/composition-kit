(ns beethoven-phrase
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.parse :as parse])

  (:use composition-kit.core)
  )

;; The first few measures of the adiago cantabile from beethoven op 13

(def drums (midi/midi-instrument 2))
(def bass  (midi/midi-instrument 3))
(def clock (tempo/constant-tempo 4 4 110)) 

(def base-beat [:c2    "X.....S.X......."
                :d2    "....Y......RY..."
                :fis2  "PR.FYR.HPR.FYR.H"
                :gis2  "..Q...Q...Q....."
                :ais2  "..............W." ])

(def fill-beat [:c2    "X.....S.X...X.Z."
                :d2    "....Y.....GJMPWZ"
                :fis2  "PR.FYR.HPR.....M"
                :gis2  "..Q...Q........."
                :ais2  "..........Z....." ])


;; So here's some basic drums. I mean, duh.
(def drum-pattern
  (-> (concatenate
       (loop-n (step-strings base-beat) 3)
       (step-strings fill-beat))
      (loop-n 2)
      (on-instrument drums)))

(def bass-line
  (-> (concatenate 
       (-> (phrase (lily "a2 c8 d b4 a1" :relative :c3))
           (loop-n 2))
       (-> (phrase (lily "b2 d8 e cis4 b1" :relative :c3))
           (loop-n 2)))
      (on-instrument bass)))

(-> (overlay drum-pattern bass-line)
    (with-clock clock)
    (midi-play))


