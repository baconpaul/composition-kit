(ns composition-kit.tempo)

;; We have a few key operations we can do on a tempo clock. Define them as multimethods switched on :clocktype
(defmulti beats-to-time        (fn [cl b] (:clocktype cl))) ;; beats since epoch. 0
(defmulti measure-beat-to-beat (fn [cl m b] (:clocktype cl))) ;; beats in measure. 1 2 3 4
(defmulti measure-beat-to-time (fn [cl m b] (:clocktype cl)))


;; Start with the simplest constant tempo clock and expand later using multimethods to get the times and so on
(defn constant-tempo [ beats-per-measure unit-of-measure bpm ]
  {:clocktype :constant
   :bpm       bpm
   :spb       (/ 60 bpm)
   :beats-per-measure beats-per-measure
   :unit-of-measure   unit-of-measure
   })

(defmethod beats-to-time        :constant [clock beats] ( * beats (:spb clock)) )
(defmethod measure-beat-to-beat :constant [clock measure beat] (+ (dec beat) ( * measure (:beats-per-measure clock))))
(defmethod measure-beat-to-time :constant [clock measure beat] (beats-to-time clock (measure-beat-to-beat clock measure beat)))

