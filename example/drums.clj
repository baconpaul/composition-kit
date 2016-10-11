(ns beethoven-phrase
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.parse :as parse])

  (:use composition-kit.core)
  )

;; The first few measures of the adiago cantabile from beethoven op 13

(def drums (midi/midi-instrument 2))
(def clock (tempo/constant-tempo 2 4 120)) ;; Will be a bit mechanical

;; So here's some basic drums. I mean, duh.
;; And here's a big bug. The loop is based on the length of the sequence not including rests. So insert rests
(midi-play
 (-> (step-strings
      :c2    "X.....S.X......."
      :d2    "....Y......RY..."
      :fis2  "PR.FYR.HPR.FYR.H"
      :gis2  "..Q...Q...Q....."
      :ais2  "..............W."
      )
     (loop-n 4)
     (on-instrument drums)
     (with-clock clock)))


