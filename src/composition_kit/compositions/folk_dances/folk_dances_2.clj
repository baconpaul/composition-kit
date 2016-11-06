(ns composition-kit.compositions.folk-dances.folk-dances-2
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))


(def piano (midi/midi-instrument 0))
(def soft-lead (midi/midi-instrument 1))
(def gunky-hit (midi/midi-instrument 2))
(def bass-inst (midi/midi-instrument 3))
(def solo-violin (midi/midi-instrument 4))

(def clock (tempo/constant-tempo 5 4 97))

(def lead
  (let [brk       true
        
        melody-structure
        [[:ees4 2 70] [:d4 3 65 brk]
         [:ees4 1 72] [:f4 1 74] [:g4 3 77 brk]
         
         [:aes4 2 80] [:g4 3 76 brk]
         [:f4 1 71] [:g4 1 76] [:ees4 1 71] [:d4 1 67] [:c4 1 64] [:d4 1 65] [:bes3 4 58]

         [:ees4 2 70] [ :f4 3 74 brk]
         [:g4 1 75] [:aes4 1 78] [:bes4 3 82 brk]

         [:aes4 2 80] [:g4 3 76 brk]
         [:f4 1 71 ] [:g4 1 76] [:ees4 1 71] [:d4 1 67] [:c4 1 64] [:d4 1 65] [:bes3 4 58 brk]

         [:bes3 2 65] [:ees4 3 60 brk]
         [:d4 2 65] [:ees4 3 60 brk]
         [:bes3 2 65] [:ees4 3 60 brk]
         [:d4 2 65] [:aes4 2 72] [:g4 1 70] [:f4 1 66] [:g4 1 68] [:ees4 2 62]
         [:d4 1 70] [:ees4 1 70] [:c4 1 68] [:d4 1 72] [:bes3 1 68 brk]

         [:bes4 2 65] [:ees5 3 60 brk]
         [:d5 2 65] [:ees5 3 60 brk]
         [:bes4 2 65] [:ees5 3 60 brk]
         [:d5 2 65] [:aes5 2 72] [:g5 1 70] [:f5 1 66] [:g5 1 68] [:ees5 2 62]
         [:d5 1 70] [:ees5 1 70] [:c5 1 68] [:d5 1 72] [:bes4 1 68 brk]

         ]

        a-to-n
        (fn [sp [pitch dur vel bk]]
          (-> sp
              (update-in [:end] + dur)
              (update-in
               [:notes]
               concat [(i/notes-with-duration pitch dur (:end sp) (if (nil? bk) (* 1.01 dur) (* 0.94 dur)))]))
          )
        ;;          (i/notes-with-duration

        music
        (->
         (ls/concrete-logical-sequence (:notes (reduce a-to-n {:end 0 :notes []} melody-structure)))
         (ls/explicit-segment-dynamics (map #(nth % 2) melody-structure))
         (ls/transpose 12)
         )
        ]
    (>>>
     (rest-for 20)
     music)
    )
  )

(defn try-out [p i]
  (-> p (ls/on-instrument i) (ls/with-clock clock) (midi-play :beat-clock clock)))

(defn blur-chord [chord duration]
  (apply <*> (map
              (fn [n delay] (>>> (rest-for delay) (ls/explicit-phrase [n] [ (- duration delay)])))
              chord
              (map (partial * 0.02) (range))
              ))
  )

(def piano-m
  (let [bc blur-chord
        chord-intro (<*>
                     (->
                      (bc [ :g2 :a2 :bes2 ] 2)
                      (ls/loop-n 10)
                      (ls/line-segment-dynamics 0 50 9.9 67 10 45 20 81)
                      (ls/transpose 12)
                      )
                     (ls/pedal-held-and-cleared-at 0 19)
                     )

        chord-part-a
        (->
         (<*>
          (->
           (>>>
            (bc [ :g2 :a2 :bes2] 2)
            (bc [ :ges2 :a2 :bes2] 3)
            
            (bc [ :g2 :a2 :bes2] 1)
            (bc [ :e2 :a2 :bes2] 1)
            (bc [ :d2 :a2 :bes2] 3)

            (bc [ :g2 :a2 :bes2] 2)
            (bc [ :ges2 :a2 :bes2] 3)
            
            (bc [ :g2 :a2 :bes2] 1)
            (bc [ :ges2 :a2 :bes2] 1)
            (bc [ :d2 :a2 :bes2] 1)
            (bc [ :e2 :a2 :bes2] 1)
            (bc [ :ges2 :a2 :bes2] 1)
            (bc [ :g2 :a2 :bes2] 1)
            (bc [ :e2 :a2 :bes2] 4)
            )
           (ls/line-segment-dynamics
            0 52
            5 54
            9.9 67
            10 55
            15 62
            20 67)
           (as-> ms
               (<*> (ls/transpose ms 12)
                    (ls/transpose ms 24)))
           )
          (ls/pedal-held-and-cleared-at 0 5 10 16 20)
          )
         (ls/loop-n 2)
         )
        
        ;;_ (try-out chord-part-a piano)
        ]
    (>>>
     chord-intro
     chord-part-a
     )
    )
  )

(def gunk-poly
  (let [g4 (fn [d] (ls/explicit-phrase [:g4] [d]))
        initial-triplets
        (-> (g4 2/3)
            (ls/loop-n (* 3 10))
            (ls/line-segment-dynamics 0 50 10 70 10.01 60 20 90)
            )

        five-measure
        (>>>
         (ls/loop-n (g4 2/3) 6)
         (g4 1)
         
         (rest-for 1/2)
         (g4 1)
         (g4 1/2)
         (ls/loop-n (g4 2/3) 3)
         (g4 1)

         (ls/loop-n (g4 2/3) 6)
         (g4 1)
         
         (rest-for 1/2)
         (ls/loop-n  (g4 1) 5)
         (g4 1/2)
         (ls/loop-n (g4 2/3) 6)
         )

        ]
    (>>>
     initial-triplets
     five-measure
     (<*>
      five-measure
      (-> five-measure
          (ls/transpose 12)
          (ls/amplify 0.6))
      )
     )
    ))

(def bass-pattern
  (let [intro (->  (ls/explicit-phrase [:g2 :ees2 :g2 nil] [10 5 4.95 0.05])
                   (ls/hold-for-pct 1)
                   )
        phrase-one (-> (lily "g2 ges2. g4 ees d2. g2 ges2. g4 ges d e ges g e1" :relative :c3)
                       (ls/hold-for-pct 1.01)
                       (ls/loop-n 2))
        ]
    (>>>
     intro
     phrase-one
     )
    ))

(def violin
  (let [gs (-> (ls/explicit-phrase [:ees5 :g5
                                    :g4 :aes5 :bes5
                                    :c5 :d5 :ees5 :f5 :g5 :aes5 :bes5
                                    :g4 :aes5 :bes5
                                    :bes5 :aes5 :g5 :f5 :ees5 :d5 :ees5 :f5
                                    ]
                                   [5 5
                                    10 2 3
                                    1 1 1 1 1 1 4
                                    10 2 3
                                    1 1 1 1 1 1 1 3
                                    ])
               (ls/hold-for-pct 1)
               (ls/explicit-dynamics 50 70
                                     65 72 67
                                     50 56 58 63 64 72 54
                                     65 72 67
                                     50 56 58 63 64 72 54 64
                                     )
               )
        ;;_ (try-out gs solo-violin)
        ]
    (>>>
     (rest-for 10)
     gs
     )
    ))

(def final-song
  (<*>
   (-> piano-m (ls/on-instrument piano))
   (-> lead (ls/on-instrument soft-lead))
   (-> gunk-poly (ls/on-instrument gunky-hit))
   (-> bass-pattern (ls/on-instrument bass-inst))
   (-> violin (ls/on-instrument solo-violin))
   )
  )

(def play-it true)
(def player
  (when play-it
    (->
     final-song
     (ls/with-clock clock)
     (midi-play
      :beat-zero -1
      :beat-end 70
      :beat-clock clock
      ))))

;;(def x (composition-kit.events.physical-sequence/stop player))

