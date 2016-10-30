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
            b4 (mod b 4)
            bm3w (mod b 3/2)]
        (cond
          (= b4 0) 105
          (= b4 3/2) 98
          (= b4 3) 104
          (= b4 7/2) 107
          :else (-  87 (* 3 bm3w))))
      )))


(def one-a-piano
  (->
   (<*>
    (->
     (lily "c8 c8 c8 c8 c8 c8 d bes'
        c,8 c8 c8 c8 c8 c8 d bes'
        c,8 c8 c8 c8 c8 c8 d bes'
        ees,8 d des c ces bes <bes' bes'>8 <bes bes'>")
     (ls/transform :dynamics accent-pattern-dynamics)
     (ls/hold-for-pct 0.7)
     )
    (->
     
     (lily  "a8. a8. a8. a8. g8 <ees bes'>8
            a8. a8. a8. a8. bes8 <ees, bes'>8
            a8. a8. a8. a8. g8 <ees bes'>8	
            des'8. c8. ces8. bes8. <bes, bes'>8 <bes bes'>8")
     (ls/transform :dynamics accent-pattern-dynamics)
     (ls/hold-for-pct 0.7)
     )
    )
   (ls/loop-n 2)
   )
  )

(def one-b-piano
  (->
   (<*>
    (->
     (lily "bes8. a g fis g8 g
bes8. a g fis g8 g
bes8. a g fis g8 g
g16 a g8 fis4 
g16 a g8 fis8 g
bes8. a g fis g8 g
bes8. a g fis g8 g
bes8. a g fis g8 g
g16 a g8 fis4 
g16 a g8 f4 
" :relative :c5)
     (ls/transform :dynamics accent-pattern-dynamics)
     (ls/hold-for-pct 0.7)
     )
    (->
     (lily "bes8. a g fis g8 a
bes8. a g fis g8 a
bes8. a g fis g8 bes
g16 a g8 fis4 
g16 a g8 fis8 g
g8. f e dis e8 f
g8. f e dis e8 f
g8. f e dis e8 f
e16 f e8 dis4 e16 f e8 d4
" :relative :c4)
     (ls/transform :dynamics accent-pattern-dynamics)
     (ls/hold-for-pct 0.7)
     )
    (-> (lily "c8 c c c c c c c")
        (ls/hold-for-pct 0.7)
        (ls/line-segment-dynamics 0 70 4 87)
        (ls/loop-n 8))
    )
   )
  )

(def final-song
  (-> 
   (->
    (>>>
     (<*>
      (-> one-a-piano (ls/on-instrument piano))
      )
     (<*>
      (-> one-b-piano (ls/on-instrument piano))
      )
     )
    (ls/loop-n 2)
    )
   (ls/with-clock clock)
   )
  )

(def play-it true)
(def player
  (when play-it
    (-> final-song
        (midi-play
         ;;:beat-zero 32
         ;;:beat-end 12
         :beat-clock clock
         ))))
  
