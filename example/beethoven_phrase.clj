(ns beethoven-phrase
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.logical-sequence :as ls])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.physical-to-logical :as ptol])
  )

;; The first few measures of the adiago cantabile from beethoven op 13

(def piano (midi/midi-instrument 0))
(def clock (tempo/constant-tempo 2 4 47)) ;; Will be a bit mechanical

(def top-theme
  (ls/line-segment-dynamics
   (ls/sequence-from-pitches-and-durations
    [:c4 :bes3 :ees4 :des4
     :c4 :ees4 :aes4 :bes4 :ees4 :e4 ]
    [1 1 3/2 1/2 1/2 1/2 1/2 1/2 3/2 1/2 ]
    :length :legato)
   0 90
   4 85
   (- 6 1/2)   93
   6 85
   8 88) )


(def bottom-theme
  (ls/line-segment-dynamics
   (ls/sequence-from-pitches-and-durations
    [:aes2 :des3 :c3 :g2
     :aes2 :g2 :f2 :f3 :ees3 :ees2 ]
    [1 1 1 1
     1/2 1/2 1/2 1/2 1 1 ])
   0 80
   4 75
   8 90))

(defn sixteenths [p1 p2 p3 p4]
  (ls/sequence-from-pitches-and-durations [p1 p2 p3 p4] [1/4 1/4 1/4 1/4])) 
(defn alternates [p1 p2] (sixteenths p1 p2 p1 p2))

(def middle-theme
  (ls/explicit-segment-dynamics
   (ls/concat-sequences
    (alternates :aes3 :ees3)
    (alternates :g3 :ees3)
    (alternates :aes3 :ees3)
    (alternates :bes3 :ees3)
    (sixteenths :aes3 :ees3 :bes3 :ees3 )
    (sixteenths :c4 :aes3 :d4 :aes3)
    (alternates :g3 :bes3)
    (alternates :g3 :bes3))
   (flatten (repeat 8 '(60 57 59 54)))))

(def pedal-sequence
  (ls/concrete-logical-sequence
   (concat
    (map #(ls/sustain-pedal-event 90 %) (map (partial + 0.04) (list 0 1 2 4 5 6)))
    (map #(ls/sustain-pedal-event 0 %) (map (partial + 0.01) (list 1 2 4 5 6 8)))
    )
   ))



(def beethoven
  (ls/loop-sequence (ls/merge-sequences top-theme bottom-theme middle-theme pedal-sequence) 1))

(def ps (-> (ps/new-sequence)
            (ptol/schedule-logical-on-physical
             (-> beethoven
                 (ls/assign-instrument piano)
                 (ls/assign-clock clock)))))

(def ps-agent (ps/play ps))




