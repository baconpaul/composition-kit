(ns composition-kit.compositions.folk-dances.folk-dances-2
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])
  (:require [composition-kit.music-lib.curves :as curves])

  (:use composition-kit.core))


;; FIXME - pedal on piano
;; FIXME - dynamics and instability on bells
;; FIXME - dynamics generally
;; FIXME - control bends on the pitch wheel for the bell in the holds at the bes4 ees1 section

(def piano (midi/midi-instrument 0))
(def soft-lead (midi/midi-instrument 1))
(def gunky-hit (midi/midi-instrument 2))
(def bass-inst (midi/midi-instrument 3))
(def solo-violin (midi/midi-instrument 4))

;; Ritardando at beat (217 )
(defn btt [clock beats]
  (if (< beats 216.5)
    (* beats (:spb clock))
    (let [slowdown  (curves/sigmoid 216.5 (:spb clock) 227 (* 1.015 (:spb clock)))
          ]
      (* beats (slowdown beats))
      )
    )
  )
(def clock (tempo/user-function-tempo 5 4 92 btt))

(def lead
  (let [brk       true
        
        melody-structure
        [
         [:ees4 2 70] [:d4 3 65 brk]
         [:ees4 1 72] [:f4 1 74] [:g4 3 77 brk]
         
         [:aes4 2 80] [:g4 3 76 brk]
         [:f4 1 71] [:g4 1 76] [:ees4 1 71] [:d4 1 67] [:c4 1 64] [:d4 1 65] [:bes3 4 58]

         [:ees4 2 70] [ :f4 3 74 brk]
         [:g4 1 75] [:aes4 1 78] [:bes4 3 82 brk]

         [:aes4 2 80] [:g4 3 76 brk]
         [:f4 1 71 ] [:g4 1 76] [:ees4 1 71] [:d4 1 67] [:c4 1 64] [:d4 1 65] [:bes3 4 58 brk]

         [:bes3 2 65] [:ees4 3 60 brk]
         [:d4 2 65] [:ees4 4 60 ]

         [:bes3 2 65] [:ees4 3 60 brk]
         [:d4 2 65] [:ees4 4 60] 

         [:bes3 2 67] [:ees4 3 60 brk]
         [:d4 2 65] [:ees4 3 60 brk]

         [:bes3 2 65] [:ees4 3 60 brk]
         [:d4 2 65] [:aes4 2 72] [:g4 1 70] [:f4 1 66] [:g4 1 68] [:ees4 3 62]
         [:d4 1 70] [:ees4 1 70] [:c4 1 68] [:d4 1 72] [:bes3 1 68 brk]

         [:bes4 2 65] [:ees5 3 60 brk]
         [:d5 2 65] [:ees5 3 60 brk]

         [:bes4 2 65] [:ees5 3 60 brk]
         [:d5 2 65] [:aes5 2 72] [:g5 1 70] [:f5 1 66] [:g5 1 68] [:ees5 3 62]
         [:d5 1 70] [:ees5 1 70] [:c5 1 68] [:d5 1 72] [:bes4 1 68 brk]

         [:aes4 2 69 ] [:g4 1 72 ] [:f4 1 74] [ :g4 1 80]
         [:ees4 1 83] [:f4 1 85] [:d4 1 87] [:ees4 1 91] [:bes3 1 97 brk]

         [:bes3 1 62] [:ees4 4 64]
         [:bes3 1 62] [:ees4 4 64 brk]

         [:bes3 1 62] [:aes4 1 64] [:g4 1 64] [:f4 1 64] [:ees4 1 64] ;; fix dynamics
         [:bes3 1 62] [:d4 1 64] [:ees4 1 64] [:c4 1 64] [:bes3 1 64 brk] ;; fix dynamics

         [:bes3 1 62] [:aes4 1 64] [:g4 1 64] [:f4 1 64] [:ees4 1 64] ;; fix dynamics
         [:bes3 1 62] [:d4 1 64] [:ees4 1 64] [:c4 1 64] [:bes3 1 64 brk] ;; fix dynamics

         [:bes3 1 62] [:ees4 4 64]
         [:bes3 1 67] [:ees4 3 72] [:bes3 1 67]

         [:ees4 2 70] [:d4 3 65 brk]
         [:ees4 1 72] [:f4 1 74] [:g4 3 77 brk]
         
         [:aes4 2 80] [:g4 3 76 brk]
         [:f4 1 71] [:g4 1 76] [:ees4 1 71] [:d4 1 67] [:c4 1 64] [:d4 1 65] [:bes3 4 58]

         ]

        a-to-n
        (fn [sp [pitch dur vel bk]]
          (-> sp
              (update-in [:end] + dur)
              (update-in
               [:notes]
               concat (if (nil? pitch)
                        [(i/rest-with-duration dur (:end sp))]
                        [(i/notes-with-duration pitch dur (:end sp) (if (nil? bk) (* 1.01 dur) (* 0.94 dur)))])))
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

        chord-part-b
        (<*>
         (->
          (>>>
           (bc [ :g2 :a2 :bes2 ] 2)
           (bc [ :ges2 :a2 :bes2 ] 3)
           (bc [ :g2 :a2 :bes2 ] 2)
           (bc [ :e2 :a2 :bes2 ] 2)
           (bc [ :e2 :a2 :bes2 ] 2)


           (bc [ :g2 :a2 :bes2 ] 2)
           (bc [ :ges2 :a2 :bes2 ] 3)
           (bc [ :g2 :a2 :bes2 ] 2)
           (bc [ :e2 :a2 :bes2 ] 2)
           (bc [ :e2 :a2 :bes2 ] 2)


           ;;(bc [ :g2 :a2 :bes2 ] 2)
           (rest-for 2) ;; beat 24 after this rest
           (bc [ :ges2 :aes2 :bes2 ] 3) ;; that aes is super important harminically
           (bc [ :f2 :aes2 :bes2 ] 2) 
           (bc [ :ees2 :f2 :bes2 ] 3) ;; 29 

           (bc [ :g2 :a2 :bes2 ] 2)
           (bc [ :ges2 :aes2 :bes2 ] 3) ;; 34
           
           (bc [ :f2 :aes2 :bes2 ] 2) 
           (bc [ :ees2 :f2 :bes2 ] 2) 
           (bc [ :ees2 :g2 :bes2 ] 2)

           (bc [ :f2 :aes2 :bes2] 2)
           (bc [ :c2 :f2 :bes2] 2)

           (bc [ :c2 :g2 :bes2] 2)
           (bc [ :c2 :f2 :bes2] 2)
           (bc [ :bes1 :f2 :bes2] 1)
           )
          (ls/line-segment-dynamics 0 50 21 62 22 50 34.9 74 35 58 52 66)
          
          (as-> ms
              (<*> (ls/transpose ms 12)
                   (ls/transpose ms 24)
                   ))
          
          )
         (ls/pedal-held-and-cleared-at 0 5 10 15 20 29 34 40 44 52)
         )

        chord-part-c
        ;; this is 8 measures of 2 beat chord to build that 2 beat feel
        ;; So 8 * 5 = 40 / 2 = 20 - 1 (for the rest) = 19 chords I need here
        (<*>
         (>>>
          (rest-for 2)
          (->
           (>>>
            (bc [:f2 :aes2 :bes2] 2)
            (bc [:ees2 :aes2 :bes2] 2)
            (bc [:ees2 :aes2 :bes2] 2)
            (bc [:ees2 :g2 :bes2] 2)

            (bc [:f2 :aes2 :bes2] 2)
            (bc [:d2 :f2 :bes2] 2)
            (bc [:ees2 :g2 :bes2] 2)

            (bc [:ees2 :f2 :bes2] 2)
            (bc [:ees2 :g2 :bes2] 2)
            (bc [:c2 :g2 :bes2] 2)
            (bc [:c2 :f2 :bes2] 2)
            (bc [:c2 :ees2 :bes2] 2)
            
            (bc [:bes1 :f2 :bes2] 2)
            (bc [:bes1 :ees2 :bes2] 2)

            (bc [:c2 :ees2 :aes2] 2)
            (bc [:d2 :f2 :bes2] 2)
            (bc [:c2 :f2 :aes2] 2)
            (bc [:bes1 :f2 :aes2] 2)
            (bc [:bes1 :ees2 :aes2] 2)
            )
           (ls/line-segment-dynamics 0 60 17 70 18 58 31 72 32 58)
           
           (as-> ms
               (<*> (ls/transpose ms 12)
                    (ls/transpose ms 24)
                    ))

           )
          )
         (ls/pedal-held-and-cleared-at 0 10 20 30 40)
         )

        held-part
        (>>>
         (-> (lily "<bes' bes' f'>4 <bes, bes' f' bes ees' f>1
<bes' bes' f'>4 <bes, bes' f' bes ees' f>1

<bes' bes' f'>4 <bes, bes' g' bes ees' f>1
<bes' bes' f'>4 <bes, bes' f' bes ees' f>1

<bes' bes' f'>4 <bes, bes' g' bes ees' f>1
<bes' bes' f'>4 <bes, bes' f' bes ees' f>1

<bes' bes' f'>4 <bes, bes' f' bes ees' f>1
<bes' bes' f'>4 <bes, bes' f' bes ees' f>1

" :relative :c2)
             ;; FIXME - make this a builtin
             (->> (mapcat (fn [c]
                            (let [p (i/item-payload c)
                                  new-item
                                  (fn [i n]
                                    (-> (i/identity-item-transformer c)
                                        (i/add-transform :payload (constantly (assoc p :notes [n])))
                                        (i/add-transform :beat (comp (partial + (* 0.01 (inc i))) i/item-beat))
                                        ))
                                  ]
                              (if (seq? (:notes p))
                                (map-indexed  new-item (:notes p))
                                [c])
                              )
                            )))

             (ls/hold-for-pct 0.999)
             ;; FIXME - these dynamics are sort of bad
             (ls/line-segment-dynamics 0 62 9.9 68 10 62 30 71 40 60)

             )
         )

        chord-part-d
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
            9.9 72
            10 64
            15 62
            20 58)
           (as-> ms
               (<*> (ls/transpose ms 12)
                    (ls/transpose ms 24)))
           )
          (ls/pedal-held-and-cleared-at 0 5 10 16 20)
          )
         )
        ;;_ (try-out held-part piano)
        ]
    (>>>
     chord-intro
     chord-part-a

     chord-part-b

     chord-part-c

     held-part

     chord-part-d
     )
    )
  )

(def gunk-poly
  (let [g4 (fn [d] (ls/explicit-phrase [:g4] [d]))
        g4n (fn [d n] (ls/loop-n  (ls/explicit-phrase [:g4] [d]) n))
        nt (fn [n d] (ls/explicit-phrase [n] [d]))
        nta (fn [n d] (ls/explicit-phrase n (repeat (count n) d)))
        ntn (fn [n d r] (ls/loop-n (nt n d) r))
        initial-triplets
        (->
         (>>>  (g4n 2/3 (* 6 3))
               (g4n 3/4 4)
               (g4n 1/2 2)
               (g4n 2/3 (* 2 3))
               )

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

        second-part
        (>>>
         (ls/loop-n (g4 2/3) 3)
         (ls/loop-n (g4 3/4) 4)
         (ls/loop-n (g4 2/3) 9)

         (ls/loop-n (g4 2/3) 3)
         (ls/loop-n (g4 3/4) 4)
         (ls/loop-n (g4 2/3) 9)

         ;;(ls/loop-n (g4 2/3) 3)
         (rest-for 2)
         (ls/loop-n (nt :aes4 3/4) 4)

         (ls/loop-n (nt :bes4 2/3) 3)
         (ls/loop-n (nt :f4 3/4) 2)
         (ls/loop-n (nt :ees4 3/4) 2)

         (ls/loop-n (g4 2/3) 3)
         (ls/loop-n (nt :aes4 3/4) 4)

         (ls/loop-n (nt :bes4 2/3) 3)
         (ls/loop-n (nt :ees4 2/3) 3)
         (ls/loop-n (nt :g4 2/3) 3)
         (ls/loop-n (nt :aes4 2/3) 3)
         (ls/loop-n (nt :f4 2/3) 3)
         (ls/loop-n (nt :c5 2/3) 3)
         (ls/loop-n (nt :c5 2/3) 3)
         
         (nt :bes4 1)

         )

        third-part
        (>>>
         (rest-for 2)
         (nta [ :f5 :aes4 :bes4 ] 2/3)
         (ntn :aes4 1/2 4)
         (ntn :aes4 2/3 3)
         (ntn :g4 2/3 3)

         (ntn :aes4 1/2 4)
         (ntn :d5 2/3 3)
         (ntn :ees5 2/3 3)

         (ntn :bes4 1/2 4)
         (ntn :g4 2/3 3)
         (ntn :c5 1/3 6)
         (ntn :c4 1/2 4)
         (ntn :c5 2/3 3)

         (ntn :bes4 2/3 6)

         (ntn :c5 1/2 4)
         (ntn :d5 2/3 3)
         (ntn :c5 1 2)

         (ntn :bes4 2/3 6)
         )

        bflat-part
        (<*>
         (>>>
          (ntn :bes4 2/3 3) (ntn :bes4 3/4 4)
          (ntn :c5 3/4 4) (ntn :d5 2/3 3)
          (ntn :ees5 1/2 4) (ntn :f5 2/6 6) (ntn :d5 1/2 2)
          (ntn :bes4 1/2 2) (ntn :bes4 2/3 6)
          (ntn :d5 1/2 2) (ntn :bes4 2/3 6)
          (ntn :ees5 1/2 2) (ntn :bes4 2/3 6)
          (ntn :bes4 1/2 2) (ntn :bes4 2/3 6)
          (ntn :bes4 1/2 2) (ntn :bes4 2/3 6)

          )
         ;; put in a pitch bend here
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
     second-part
     third-part
     bflat-part
     five-measure
     )
    ))

(def bass-pattern
  (let [intro (->  (ls/explicit-phrase [:g2 :ees2 :g2 nil] [10 5 4.95 0.05])
                   (ls/hold-for-pct 1.01)
                   )
        phrase-one (-> (lily "g2 ges2. g4 ees d2. g2 ges2. g4 ges d e ges g e1" :relative :c3)
                       (ls/hold-for-pct 1.01)
                       )

        phrase-two (-> (lily "g2 ges2. g2 ees1  g2 ges2. g2 ees1
 r2 ges2. aes2 ees2. g2 aes2. bes2 ees,2 g aes f c1 bes2.
f'2 ees1. f2 d2 ees1. c1. bes1 c2 d2 c2 bes1

bes'4 bes,1 bes'4 bes,1 
bes'4 bes,1 bes'4 bes,1 bes'4 bes,1
bes'4 bes,1 bes'4 bes,1 bes'4 bes,1

 " :relative :c3)
                       (ls/hold-for-pct 1.01)) ;; FIX - implement ties in lily parser and set this back to 1.01 or kill that ees

        phrase-three (-> (lily "bes2 ges2. g4 ees d2. g2 ges2. g4 ges d e ges g e1" :relative :c3)
                         (ls/hold-for-pct 1.01)
                         )

        ]
    (>>>
     intro
     phrase-one phrase-one
     phrase-two
     phrase-three
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
        gs-b (-> (ls/explicit-phrase [:bes4 :ees5
                                      :d5 :ees5
                                      :bes4 :ees5
                                      :d5 :ees5
                                      :f5 :g5 :aes5 :bes5
                                      :g5 :aes5 :bes5 :c6 :bes5
                                      :aes5 :bes5 :g5 :f5 :g5 :ees5 :f5 :bes5
                                      ]
                                     [2 3
                                      2 4
                                      2 3
                                      2 4

                                      2 3 2 3
                                      2 3 2 2 1

                                      1 1 3 1 1 1 1 1
                                      ]
                                     )
                 (ls/hold-for-pct 1)
                 (ls/explicit-dynamics 60 66
                                       60 71
                                       60 66
                                       63 74
                                       71 74 77 82
                                       72 76 60 62 64 68 74 64
                                       )
                 )

        gs-c (-> (ls/explicit-phrase [ :bes4 :c5 :d5 :ees5
                                      :f5 :g5 :d5 :aes5 :g5
                                      :f5 :g5 :aes5 :bes5
                                      :aes4
                                      ]
                                     [
                                      2 3 2 3
                                      2 3 2 2 1
                                      1 1 3 5
                                      10
                                      ]
                                     )
                 (ls/hold-for-pct 1)
                 (ls/explicit-dynamics 71 79 74 82
                                       71 79 77 87 83
                                       82 87 88 75
                                       72
                                       ))

        gs-d (-> (ls/explicit-phrase [:bes4 :ees5
                                      :bes4 :ees5
                                      :bes4 :aes5 :g5 :f5 :ees5
                                      :bes4 :d5 :ees5 :c5 :bes4

                                      :bes4 :c6 :bes5 :aes5 :g5
                                      :bes4 :f5 :g5 :ees5 :d5

                                      :d5 :ees5 :f5 :g5 :aes5

                                      :bes5 :aes5
                                      :g5 :aes5 :bes5
                                      :c6

                                      :f5 :g5 :aes5 :bes5 :c5 :d5 :bes4
                                      ]
                                     [3 2
                                      3 2                                      
                                      1 1 1 1 1
                                      1 1 1 1 1

                                      1 1 1 1 1
                                      1 1 1 1 1

                                      2 2 2 2 2

                                      2 3
                                      1 1 3
                                      5

                                      1 1 1 1 1 1 4
                                      ])
                 (ls/hold-for-pct 1)
                 (ls/explicit-dynamics 71 79
                                       71 82
                                       74 87 84 76 73
                                       74 87 84 76 73

                                       74 87 84 76 73
                                       74 87 84 76 73

                                       77 82 84 87 91

                                       85 78 77 79 82
                                       84

                                       87 86 85 83 81 78 73
                                       )
                 )
        
        ;;_ (try-out gs-b solo-violin)
        ]
    (>>>
     (rest-for 10)
     gs
     gs-b
     gs-c
     gs-d
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

;;(-> gunk-poly (ls/on-instrument gunky-hit) (ls/with-clock clock) (midi-play))
;;(-> lead (ls/on-instrument soft-lead) (ls/with-clock clock) (midi-play :beat-zero 19))

(def play-it true)
(def player
  (when play-it
    (->
     final-song
     (ls/with-clock clock)
     (midi-play
      :beat-zero -1
      ;;:beat-end 123
      :beat-clock clock
      ))))

;;(def x (composition-kit.events.physical-sequence/stop player))

(/ (float  (tempo/beats-to-time clock  (ls/beat-length final-song))) 60 )
(ls/beat-length final-song)

