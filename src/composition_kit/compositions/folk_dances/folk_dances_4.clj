(ns composition-kit.compositions.folk-dances.folk-dances-4
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

(def piano (midi/midi-instrument 0))
(def mallet-two (midi/midi-instrument 1))

(def clock (tempo/constant-tempo 4 4 152))

(def accent-pattern-dynamics
  (fn [i]
    (fn [w]
      (let [b (i/item-beat w)
            b4 (mod b 4)]
        (cond
          (= b4 0) 105
          (= b4 3/2) 98
          (= b4 3) 104
          (= b4 7/2) 107
          :else 87))
      )))
    

(def first-piano
  (<*>
   (-*>
    (lily "c8 c8 c8 c8 c8 c8 d bes'
        c,8 c8 c8 c8 c8 c8 d bes'
        c,8 c8 c8 c8 c8 c8 d bes'
        ees,8 d des c ces bes <bes' bes'>8 <bes bes'>")
    (ls/transform :dynamics accent-pattern-dynamics)
    (ls/hold-for-pct 0.7)
    )
   (-*>
    
    (lily  "a8. a8. a8. a8. g8 <ees bes'>8
            a8. a8. a8. a8. bes8 <ees, bes'>8
            a8. a8. a8. a8. g8 <ees bes'>8	
            des'8. c8. ces8. bes8. <bes, bes'>8 <bes bes'>8")
    (ls/transform :dynamics accent-pattern-dynamics)
    (ls/hold-for-pct 0.7)
    )
   )
  )


(def play-it true)
(def player
  (when play-it
    (->
     (<*>
      (-> first-piano (on-instrument piano))
      )
     (with-clock clock)
     (midi-play))))
  
