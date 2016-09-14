(ns piano-phase
  (require [composition-kit.midi-util :as midi])
  (require [composition-kit.tempo :as tempo])
  (require [composition-kit.logical-sequence :as ls])
  )

;; An implementation of the first section of Reich's piano phase.

(def piano-one (midi/midi-instrument 0))
(def piano-two (midi/midi-instrument 1))

(def clock-one (tempo/constant-tempo 3 4 80)) ;; constrant tempo
(def clock-two (tempo/multi-segment-constant-tempi 3 4
                                                   0 80
                                                   10 83)) ;; line seg tempo. For now speed up after 10 repetitions

;; Here's the reich pattern, all as 16th notes
(def pitch-pattern [ :e4 :fis4 :b4 :cis5 :d5 :fis4 :ef :cis5 :b4 :fis4 :d5 :cis5 ] )
(def duration-pattern (map #( / % 4 ) (range 12)))

;; FIXME - the volume is ignored right now
(def reich-pattern-leg
  (ls/sequence-from-pitches-and-durations
   pitch-pattern
   duration-pattern
   :length :legato
   :volume 80)) ;; later expand these phrase dynamics

(def reich-pattern-stac
  (ls/sequence-from-pitches-and-durations
   pitch-pattern
   duration-pattern
   :length :staccato
   :volume 90)) ;; later expand these phrase dynamics

(def ps physical-sequence)

(ptol/schedule-logical-on-physical
 ps
 (ls/loop-sequence reich-pattern-leg 50)
 piano-one
 clock-one)

(ptol/schedule-logical
 ps
 (ls/loop-sequence reich-pattern-stac 50)
 piano-two
 clock-two)

(ps/play ps)
