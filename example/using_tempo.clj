(ns composition-kit.example.using-tempo
  (:require [composition-kit.tempo :as t]))

;; The tempo objects allow you to translate from time signatures and tempi to actual relative time. The most basic use is
;; to set up a constant beat per minute time signature and them ask for a measure and beat time; or a beats time; or a beats
;; measure and in-measure beat
(let [clock (t/constant-tempo 3 4 120)]
  {
   :beat117              (t/beats-to-time clock 3)
   :meas4_beat2_as_beats (t/measure-beat-to-beat clock 4 2)
   :meas4_beat2          (t/measure-beat-to-time clock 4 2.5) 
   }) 


