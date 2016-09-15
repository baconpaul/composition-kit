(ns composition-kit.test-phys-to-log
  (use clojure.test)
  (:require [composition-kit.logical-sequence :as ls])
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.physical-to-logical :as ptol])
  )

(deftest simple-conversion
  (let [phrase (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 ] [ 1 1/2 1/2 ] )
        inst   (midi/midi-instrument 0)
        clock  (tempo/constant-tempo 2 4 140)
        pseq   (ptol/schedule-logical-on-physical (ps/new-sequence) phrase inst clock)
        ]
    (is (= (count (:seq pseq)) 6)) ;; Note on and note off events
    )
  )



