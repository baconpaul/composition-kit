(ns drums-example
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])

  (:use composition-kit.core)
  )

;; The first few measures of the adiago cantabile from beethoven op 13

(def drums (midi/midi-instrument 2))
(def bass  (midi/midi-instrument 3))
(def clock (tempo/constant-tempo 4 4 115)) 

(def base-beat [:c2    "X.....S.X......."
                :d2    "....Y......RY..."
                :fis2  "PR.FYR.HPR.FYR.H"
                :gis2  "..Q...Q...Q....."
                :ais2  "..............W." ])

(def fill-beat [:c2    "X.F...S.X...X.Z."
                :d2    "....Y.....GJMPWZ"
                :fis2  "PR.FYR.HPR.....M"
                :gis2  "..Q...Q........."
                :ais2  "..........Z....." ])

(def fill-beat-alt [:c2    "X..O..S.X...Z.Z."
                    :d2    "....Y........XBP"
                    :fis2  "PR.FYR.HPR.....A"
                    :gis2  "..Q...Q...QQ...."
                    :ais2  "............Y..." ])


;; So here's some basic drums. I mean, duh.
(def drum-pattern 
  (-> (concatenate
       (loop-n (step-strings base-beat) 3)
       (step-strings fill-beat)
       (loop-n (step-strings base-beat) 3)
       (step-strings fill-beat-alt))

      (on-instrument drums)))

(-> drum-pattern
    (with-clock clock)
    (midi-play))


