(ns piano-phase
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.logical-sequence :as ls])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.physical-to-logical :as ptol])
  )

;; The first few measures of the adiago cantabile from beethoven op 13

(def piano (midi/midi-instrument 0))
(def clock (tempo/constant-tempo 2 4 14)) ;; Will be a bit mechanical

(def top-theme
  (ls/sequence-from-pitches-and-durations
   [:c4 :bes3 :ees4 :des4
    :c4 :ees4 :aes4 :bes4 :ees4 :e4 ]
   [1/4 1/4 3/8 1/8 1/8 1/8 1/8 1/8 3/8 1/8 ]
   :length :legato))

(def bottom-theme
  (ls/sequence-from-pitches-and-durations
   [:aes2 :des3 :c3 :g2
    :aes2 :g2 :f2 :f3 :ees3 :ees2 ]
   [1/4 1/4 1/4 1/4
    1/8 1/8 1/8 1/8 1/4 1/4 ]))

(defn sixteenths [p1 p2 p3 p4]
  (ls/sequence-from-pitches-and-durations [p1 p2 p3 p4] [1/16 1/16 1/16 1/16])) 
(defn alternates [p1 p2] (sixteenths p1 p2 p1 p2))

(def middle-theme
  (ls/concat-sequences
   (alternates :aes3 :ees3)
   (alternates :g3 :ees3)
   (alternates :aes3 :ees3)
   (alternates :bes3 :ees3)
   (sixteenths :aes3 :ees3 :bes3 :ees3 )
   (sixteenths :c4 :aes3 :d4 :aes3)
   (alternates :g3 :bes3)
   (alternates :g3 :bes3)))



(def ps (reduce
         (fn [pseq ls] (ptol/schedule-logical-on-physical pseq ls piano clock))
         (ps/new-sequence)
         [ top-theme middle-theme bottom-theme]))
;;[ middle-theme ] ))

(def ps-agent (ps/play ps))

