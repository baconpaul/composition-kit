(ns composition-kit.compositions.folk-dances.folk-dances-4
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

;; drum smidge
;; then back to piano with bass and drum one and on interwoven down continue
;; end it with an extended bersion of the mixed beat descent with triplets layered over in a growing descent pattern to a big double hit

(def instruments
  (-> (midi/midi-instrument-map)
      (midi/add-midi-instrument :piano       (midi/midi-port 0))
      (midi/add-midi-instrument :drum-set    (midi/midi-port 1))
      (midi/add-midi-instrument :syn-bass    (midi/midi-port 2))
      (midi/add-midi-instrument :muted-bell  (midi/midi-port 3))
      (midi/add-midi-instrument :violin-marc (midi/midi-port 4))
      (midi/add-midi-instrument :violin-stac (midi/midi-port 5))
      (midi/add-midi-instrument :violin-lyr  (midi/midi-port 6))
      (midi/add-midi-instrument :wierdo-lead  (midi/midi-port 7))
      )
  )

(defn on-inst [s i] (ls/on-instrument s (i instruments)))

;;(try-out (lily "c4 d e") violin-marc)

(def clock (tempo/constant-tempo 4 4 152))

(defn try-out [p i]
  (-> p (on-inst i) (ls/with-clock clock) (midi-play :beat-clock clock)))

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

(def accent-pattern-dynamics-two
  (fn [i]
    (fn [w]
      (let [b (i/item-beat w)
            b4 (mod b 4)
            ]
        (cond
          (= b4 0) 105
          (= b4 3/2) 98
          (= b4 5/2) 107
          :else 87))
      )))
(def accent-pattern-dynamics-three
  (fn [i]
    (fn [w]
      (let [b (i/item-beat w)
            b4 (mod b 4)
            ]
        (cond
          (= b4 0) 105
          (= b4 3/2) 98
          (= b4 5/2) 107
          (>= b4 3) 110
          :else 87))
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

(def two-piano
  (let [
        lh (->  (lily "f8.*92 g8.*74 a4*79 bes4.*82 c,8.*92 e*84 g4*78 a4.*76
f,8.*92 g8.*74 a4*79 bes4.*82 c8.*92 e*84 g4*78 a8*80 <c, c'>8 <c c'>8
" :relative :c3)
                (as-> ms
                    (<*> ms (ls/transpose ms -12)))
                (ls/hold-for-pct 0.99)
                )
        rh (>>>
            (->  (lily "c8 f e f a bes f g e g c g bes a f c" :relative :c4)
                 (ls/transform :dynamics accent-pattern-dynamics-two)
                 (ls/amplify 0.9)
                 (ls/hold-for-pct 0.7))
            (->  (lily "c8 f e f a bes f g e g c g bes a <c, g c'> <c g c'>" :relative :c4)
                 (ls/transform :dynamics accent-pattern-dynamics-three)
                 (ls/amplify 0.94)
                 (ls/hold-for-pct 0.7))
            )

        p (<*> lh rh)

        lh-q (->  (lily "f8.*92 g8.*74 a4*79 bes4.*82 c,8.*92 e*84 g4*78 a4.*76
f,8.*92 g8.*74 a4*79 bes4.*82 c8.*92 e*84 g4*78 a8*80 <c, c'>8 r8
" :relative :c3)
                  (as-> ms
                      (<*> ms (ls/transpose ms -12)))
                  (ls/hold-for-pct 0.99)
                  )
        rh-q (>>>
              (->  (lily "c8 f e f a bes f g e g c g bes a f c" :relative :c4)
                   (ls/transform :dynamics accent-pattern-dynamics-two)
                   (ls/amplify 0.9)
                   (ls/hold-for-pct 0.7))
              (->  (lily "c8 f e f a bes f g e g c g bes a <c, g c'>8 r8" :relative :c4)
                   (ls/transform :dynamics accent-pattern-dynamics-three)
                   (ls/amplify 0.94)
                   (ls/hold-for-pct 0.7))
              )

        ;;_ (try-out p :piano)        
        ]
    (>>>
     p
     p
     p
     p
     p
     p
     (<*> lh-q rh-q)
     (rest-for 4) (lily "<c c' c' g' c>8 <c c' c' g' c>" :relative :c2)
     (rest-for 4) (lily "<c c' c' g' c>8 <c c' c' g' c> <c c' c' g' c>" :relative :c2)
     )))


(def drum-pattern
  (let [test-p (ls/explicit-phrase [ :c4 :c4 ] [1 1])
        two-beat-accent (<*>
                         (->  (ls/explicit-phrase [ :c1 :c1 ] [ 1/2 1/2 ])
                              (ls/explicit-dynamics '( 120 110)))
                         (>>>
                          (rest-for 0.05)
                          (->  (ls/explicit-phrase [ :d1 :d1 ] [ 0.45 1/2 ])
                               (ls/explicit-dynamics '( 120 110))))
                         (->  (ls/explicit-phrase [ :cis1 :cis1 ] [ 1/2 1/2 ])
                              (ls/explicit-dynamics '( 120 110)))

                         )
        
        one-beat-accent (<*>
                         (->  (ls/explicit-phrase [ :c1  ] [ 1/2])
                              (ls/explicit-dynamics '( 120 )))
                         (>>>
                          (rest-for 0.05)
                          (->  (ls/explicit-phrase [ :d1  ] [ 0.95])
                               (ls/explicit-dynamics '( 120))))
                         (->  (ls/explicit-phrase [ :cis1  ] [ 1 ])
                              (ls/explicit-dynamics '( 120 )))

                         )

        intro  (>>> (rest-for (+ 4 4 4 3))
                    two-beat-accent
                    (->
                     (<*>
                      (ls/explicit-phrase (repeat 20 :c4) (repeat 20 3/4))
                      (ls/explicit-phrase (repeat 15 :fis2) (repeat 15 1))
                      (-> (ls/explicit-phrase (repeat 4 :c2) [ 4 4 4 2])
                          (ls/hold-for 0.2))
                      )
                     (ls/line-segment-dynamics 0 70 3.9 90 4 74 7.8 95 8 80 15 120)
                     )
                    two-beat-accent
                    )

        beat-one  [:c4    "X.FJX.W.X.FMP.Z." 
                   :fis2  "P...P...R...M..."
                   :c1    "X...........Q..."
                   ]

        beat-two  [:c4    "XPM.Z..FXPM.ZCGP" ;; bass drum
                   :fis2  "P...P...R...M..."
                   :c1    "X.......Q......."
                   ]

        beat-two-alt  [:c4    "XPM.Z.Z.W.X.YMZ." ;; bass drum
                       :fis2  "P...P...X..FM.Z."
                       :c1    "X.......Q......."
                       ]

        ;;_ (try-out (ls/loop-n  (step-strings beat-two-alt) 4) drum-set)


        intro-back [:c4    "X..FXMP.Z.FGZ..P"
                    :fis2  "P...P...R...M..."
                    :c1    "X...........Q..."
                    ]

        intro-back-alt [:c4   "CFMPZ.FMZ.FM"
                        :fis2 "P...M.P.R.X."
                        :c1   "Z..........."
                        ] ;; only 3 beats here please

        first-sec
        (->
         (>>>
          (->  (step-strings beat-one)
               (ls/loop-n 3)
               )
          (step-strings beat-two)
          (->  (step-strings beat-one)
               (ls/loop-n 3)
               )
          (step-strings beat-two-alt)
          )
         )
        

        intro-repeat
        (->
         (>>>
          (-> (step-strings intro-back)
              (ls/loop-n 3))
          (step-strings intro-back-alt)
          two-beat-accent
          )
         (ls/loop-n 2)
         )

        b-part-intro
        (>>> (rest-for (+ 4 4 4 3))
             two-beat-accent)

        b-pattern-one
        [:c1   "X.....X........."
         :e6   ".FGY...G....M..."
         :f6   "....P...G.Z....A"
         :c4   ".....Q...R...RZ."
         :fis2 "G...U...R...P..." 
         ]

        b-pattern-one-end
        [:c1   "X.....X....."
         :e6   ".FGY...G...."
         :f6   "....P...G.Z."
         :c4   ".....Q...R.."
         :fis2 "G...U...R..." 
         ]
        

        b-part-first
        (>>>
         b-part-intro

         (->  (>>> 
               (-> (step-strings b-pattern-one) (ls/loop-n 3))
               (step-strings b-pattern-one-end)
               two-beat-accent)
              (ls/loop-n 5)

              )
         (-> (step-strings b-pattern-one) (ls/loop-n 3))
         (step-strings b-pattern-one-end)
         one-beat-accent
         )
        ;;_
        ;;(try-out intro drum-pattern)
        ;;_ (try-out (ls/loop-n  (step-strings b-pattern-one) 2) :drum-set)
        ]
    
    (>>> 
     intro
     first-sec
     intro-repeat
     first-sec
     b-part-first
     (rest-for 4)
     two-beat-accent
     (rest-for 4)
     two-beat-accent
     one-beat-accent
     )


    ))

;; ROugh first idea
(def bass
  (let [
        ]
    (>>>
     (rest-for (+ 4 4 4 3))
     (->  (>>> 
           (lily "bes8 bes" :relative :c3)
           (lily "f4. g4. bes4 f4. g4. bes4 f4. g4. bes4 r2. bes4" :relative :c2)
           (lily "c1 c1 c1 c1 c1 c1 c1 c2 e2" :relative :c3)
           (lily "f4. g4. bes4 f4. g4. bes4 f4. g4. bes4 r2. bes4" :relative :c2)
           (lily "f4. g4. bes4 f4. g4. bes4 f4. g4. bes4 r2. bes4" :relative :c2)
           (lily "c1 c1 c1 c1 c1 c1 c1 c2 e2" :relative :c3)
           (lily "f8 r8 r2. r1 r1 r2. c8 c," :relative :c3)
           (lily "f8.*92 g8.*74 a4*79 bes4.*82 c,8.*92 e*84 g4*78 f4.*76
f,8.*92 g8.*74 a4*79 bes4.*82 c8.*92 e*84 g4*78 f8*80 <c, c'>8 <c c'>8" :relative :c2)
           (lily "f8.*92 g8.*74 a4*79 bes4.*82 c,8.*92 e*84 g4*78 f4.*76
f,8.*92 g8.*74 a4*79 bes4.*82 c8.*92 e*84 g4*78 f8*80 <c, c'>8 <c c'>8" :relative :c2)
           (lily "f8.*92 g8.*74 a4*79 bes4.*82 c,8.*92 e*84 g4*78 f4.*76
f,8.*92 g8.*74 a4*79 bes4.*82 c8.*92 e*84 g4*78 f8*80 <c, c'>8 <c c'>8" :relative :c2)
           (lily "f8.*92 g8.*74 a4*79 bes4.*82 c,8.*92 e*84 g4*78 f4.*76
f,8.*92 g8.*74 a4*79 bes4.*82 c8.*92 e*84 g4*78 f8*80 <c, c'>8 <c c'>8" :relative :c2)
           (lily "f8.*92 g8.*74 a4*79 bes4.*82 c,8.*92 e*84 g4*78 f4.*76
f,8.*92 g8.*74 a4*79 bes4.*82 c8.*92 e*84 g4*78 f8*80 <c, c'>8 <c c'>8" :relative :c2)
           (lily "f8.*92 g8.*74 a4*79 bes4.*82 c,8.*92 e*84 g4*78 f4.*76
f,8.*92 g8.*74 a4*79 bes4.*82 c8.*92 e*84 g4*78 f8*80 <c, c'>8 r8" :relative :c2)
           (lily "r1 c8 c8 r1 c8 c8 c8" :relative :c2)
           )
          (ls/hold-for-pct 0.96))
     )
    )
  )


(def bell-one
  (let [pat (lily "bes8. a g fis g8 g
bes8. a g fis g8 g
bes8. a g fis g8 g
g16 a g8 fis4 
g16 a g8 fis8 g
bes8. a g fis g8 g
bes8. a g fis g8 g
bes8. a g fis g8 g
g16 a g8 fis4 
g16 a g8 f4 
" :relative :c6)
        blur-pat (<*> pat
                      (-> (ls/transpose pat 12)
                          (ls/transform :beat (fn [u] (+ (i/item-beat u) 0.125)))
                          (ls/amplify 0.7)
                          )
                      (-> (ls/transpose pat 7)
                          (ls/transform :beat (fn [u] (+ (i/item-beat u) 0.0625)))
                          (ls/amplify 0.4)
                          )
                      )
        ;;_ (try-out blur-pat muted-bell)
        ]
    (>>>
     (rest-for 32)
     blur-pat
     (rest-for 32)
     blur-pat)
    )
  )

;; Interesting - first time I've had to mix two instruments. Lets do it the hard way then think aout the idiom as I approach
;; the symphonic work
(def violin
  (let [part-b-first-time
        (>>>
         (->  (lily "^inst=violin-marc ^hold=1 c2.*55 ^inst=violin-stac ^hold=0.8 c8*67 c*82" :relative :c6 :instruments instruments)
              (ls/loop-n 7))

         ;; Truplets into lily would be awesome eh?
         (-> (ls/explicit-phrase [ :c5 :d5 :e5 :d5 :e5 :f5 :f5 :g5 :a5 :a5 :b5 :c6]
                                 [ 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3]
                                 )
             (ls/hold-for-pct 0.7)
             (on-inst :violin-stac)
             )
         )

        part-a-second-time-round 
        (lily "^inst=violin-marc ^hold=1.01 f2 r8 
               ^inst=violin-stac ^hold=0.7 f8*75 f16*92 d*81 c8*94
               ^inst=violin-marc ^hold=1.01 f2*97 r2 f2*94 r8
               ^inst=violin-stac ^hold=0.7 f8 f16 d c8 ees d des c b bes
               ^inst=violin-marc bes'4*97 
              " :relative :c5 :instruments instruments)

        part-c
        (lily "^inst=violin-marc ^hold=1.01 f4*104" :relative :c5 :instruments instruments)

        b-part-solo
        (lily "^inst=violin-marc ^hold=1.01 
c1*110
f1*114
g1*117
^inst=violin-stac ^hold=0.6  \\tuplet 2/3 { a,8*80 bes c*83 bes c d*90 c d e*97 f g a }
^inst=violin-marc ^hold=1.01
bes4*105 c2*108  d4*88
^inst=violin-stac ^hold=0.8 \\tuplet 2/3 {c4 d8 c4 bes8 a4 bes8 a8 g f}
^inst=violin-marc ^hold=1.01 g,4*108 a2.*110

" :relative :c5 :instruments instruments)

        smidge-theme
        (lily "
^inst=violin-marc ^hold=0.9  c4. c4 ^inst=violin-stac ^hold=0.9 c16 bes a8 g e c
^inst=violin-marc ^hold=0.9  c'4. c4 ^inst=violin-stac ^hold=0.9 c16 bes a8 g e c c'*108
"
              :relative :c6 :instruments instruments)
        
        ]
    (>>>
     (rest-for 32)
     part-b-first-time
     part-a-second-time-round
     part-a-second-time-round
     part-b-first-time
     part-c
     (rest-for 15) ;; since part c has the final accent in it
     b-part-solo
     (rest-for 36)
     b-part-solo
     (rest-for 4)
     smidge-theme
     )
    )
  )

(def wierdo-b-lead
  (let [arp (fn [& args]
              (ls/explicit-phrase args (repeat (* 1 (count args)) 1/4))
              )
        pre-hit-arp (arp :c5 :d5 :a5 :bes5
                         :c6 :bes5 :c6 :e6
                         :f6 :g6 :d6 :f5
                         :d6 :bes5 :a5 :g5
                         
                         :c5 :c6 :c5 :d6
                         :e6 :d6 :c6 :e6
                         :f6 :d6 :bes5 :f5
                         :g5 :a5 :bes5 :d6

                         :c5 :d5 :a5 :bes5
                         :c6 :bes5 :c6 :e6
                         :f6 :g6 :d6 :f5
                         :d6 :bes5 :a5 :g5
                         
                         :c5 :c6 :c5 :d6
                         :e6 :d6 :c6 :e6
                         :f6 :d6 :bes5 :f5
                         )
        pat-arp
        (>>>
         pre-hit-arp
         (ls/explicit-phrase [:c6 :c6] [ 1/2 1/2 ])
         )

        pat-arp-b
        (>>>
         pre-hit-arp
         (ls/explicit-phrase [:c6] [ 1/2 ])
         (rest-for 1/2)
         )

        ;;_ (try-out pat-arp :wierdo-lead)

        ]
    (>>>
     (rest-for 176)
     ;; this is the right idea. Figure out the chords I want to use; and also figure out the sound a bit more "filtery"
     pat-arp
     pat-arp
     pat-arp
     pat-arp-b
     )))


;;(try-out bell-one muted-bell)

(def final-song
  (->
   (<*>
    (>>>
     (->
      (>>>
       (-> one-a-piano (on-inst :piano))
       (-> one-b-piano (on-inst :piano))
       )
      (ls/loop-n 2)
      )
     (-> two-piano (on-inst :piano))
     (-> one-a-piano (on-inst :piano))
     )
    (-> drum-pattern (on-inst :drum-set))
    (-> bass (on-inst :syn-bass))
    (-> bell-one (on-inst :muted-bell))
    violin
    (-> wierdo-b-lead (on-inst :wierdo-lead))
    )
   (ls/with-clock clock)
   )
  )

(def play-it false)
(def player
  (when play-it
    (-> final-song
        (midi-play
         :beat-zero -1
         ;;:beat-zero 232
         ;;:beat-end 184
         ;;:beat-zero 30
         ;;:beat-zero 14
         ;;:beat-end 33
         :beat-clock clock
         ))))



