(ns composition-kit.compositions.folk-dances.folk-dances-4
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

(def piano (midi/midi-instrument 0))
(def drum-set (midi/midi-instrument 1))

(def clock (tempo/constant-tempo 4 4 152))

(defn try-out [p i]
  (-> p (ls/on-instrument i) (ls/with-clock clock) (midi-play)))

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

(def drums
  {:boomy-bass  :c1
   :stick-hit   :cis1
   })

(def drum-pattern
  (let [test-p (ls/explicit-phrase [ :c4 :c4 ] [1 1])
        intro  (>>> (rest-for (+ 4 4 4 3))
                    (<*>
                     (->  (ls/explicit-phrase [ :c1 :c1 ] [ 1/2 1/2 ])
                          (ls/explicit-dynamics '( 120 110)))
                     (>>>
                      (rest-for 0.05)
                      (->  (ls/explicit-phrase [ :d1 :d1 ] [ 0.45 1/2 ])
                           (ls/explicit-dynamics '( 120 110))))
                     (->  (ls/explicit-phrase [ :cis1 :cis1 ] [ 1/2 1/2 ])
                          (ls/explicit-dynamics '( 120 110)))
                     )

                    (->
                     (<*>
                      (ls/explicit-phrase (repeat 20 :c4) (repeat 20 3/4))
                      (ls/explicit-phrase (repeat 15 :fis2) (repeat 15 1))
                      (-> (ls/explicit-phrase (repeat 4 :c2) [ 4 4 4 2])
                          (ls/hold-for 0.2))
                      )
                     (ls/line-segment-dynamics 0 70 3.9 90 4 74 7.8 95 8 80 15 120)
                     )
                    (<*>
                     (->  (ls/explicit-phrase [ :c1 :c1 ] [ 1/2 1/2 ])
                          (ls/explicit-dynamics '( 120 110)))
                     (>>>
                      (rest-for 0.05)
                      (->  (ls/explicit-phrase [ :d1 :d1 ] [ 0.45 1/2 ])
                           (ls/explicit-dynamics '( 120 110))))
                     (->  (ls/explicit-phrase [ :cis1 :cis1 ] [ 1/2 1/2 ])
                          (ls/explicit-dynamics '( 120 110)))
                     )
                    )

        beat-one  [:c1    "X...........X..." ;; bass drum
                   :fis2  "Q..P.....M....N."
                   :ais2  "......T.....V..."
                   ]
        beat-two [:c1    "X...........X..." ;; bass drum
                  :fis2  "Q...Q...Q...Q.Q."
                  ]

        first-sec
        (<*>
         (->
          (>>>
           (->  (step-strings beat-one)
                (ls/loop-n 3)
                )
           (step-strings beat-two)
           )
          (ls/loop-n 2)
          )
         )

        ;;_ (try-out (step-strings beat-one) drum-set)
        ;;_
        ;;(try-out intro drum-pattern)
        ]
    
    (>>> 
     intro
     first-sec
     )


    ))


(def final-song
  (->
   (<*>
    (->
     (>>>
      (-> one-a-piano (ls/on-instrument piano))
      (-> one-b-piano (ls/on-instrument piano))
      )
     (ls/loop-n 2)
     )
    (-> drum-pattern (ls/on-instrument drum-set))
    )
   (ls/with-clock clock)
   )
  )

(def play-it false)
(def player
  (when play-it
    (-> final-song
        (midi-play
         ;;:beat-zero 32
         ;;:beat-zero 30
         :beat-end 64
         :beat-clock clock
         ))))
  
