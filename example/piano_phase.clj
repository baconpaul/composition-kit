(ns piano-phase
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.physical-sequence :as ps])
  (:require [composition-kit.music-lib.logical-to-physical :as ptol])
  )

;; FIXME - redo with compact notation and dynamics

;; An implementation of the first section of Reich's piano phase.

(def piano-one (midi/midi-instrument 0))
(def piano-two (midi/midi-instrument 1))

(def clock-one (tempo/constant-tempo 3 4 140)) ;; constrant tempo
(def clock-two (tempo/multi-segment-constant-tempi 3 4
                                                   1 140
                                                   10 143)) ;; line seg tempo. For now speed up after 10 repetitions

;; Here's the reich pattern, all as 16th notes
(def pitch-pattern [ :e4 :fis4 :b4 :cis5 :d5 :fis4 :e4 :cis5 :b4 :fis4 :d5 :cis5 ] )
(def duration-pattern (repeat 12 1/4))

(def loop-count 60)

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

(def ps-agent
  (let [ns (ps/new-sequence)
        s1 (ptol/schedule-logical-on-physical
            ns
            (ls/loop-sequence reich-pattern-leg loop-count)
            piano-one
            clock-one)
        s2 (ptol/schedule-logical-on-physical
            s1
            (ls/loop-sequence reich-pattern-stac loop-count)            
            piano-two
            clock-two)]
    (ps/play s2)
    )
  )



;;(ps/stop ps-agent)

