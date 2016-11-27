(ns composition-kit.compositions.folk-dances.folk-dances-4
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

(def instruments
  (-> (midi/midi-instrument-map)
      (midi/add-midi-instrument :piano       (midi/midi-port 0))
      (midi/add-midi-instrument :drum-set    (midi/midi-port 1))
      (midi/add-midi-instrument :syn-bass    (midi/midi-port 2))
      (midi/add-midi-instrument :muted-bell  (midi/midi-port 3))
      (midi/add-midi-instrument :violin-marc (midi/midi-port 4))
      (midi/add-midi-instrument :violin-stac (midi/midi-port 5))
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
  (let [lh (->  (lily "f4. a2 r8 bes4. g2 r8" :relative :c3)
                (as-> ms
                    (<*> ms (ls/transpose ms -12)))
                (ls/hold-for-pct 0.99)
                )
        rh (->  (lily "<f a c>1 <f bes c>1" :relative :c3) (ls/hold-for-pct 0.99))

        p (<*> lh rh)
        ;;_ (try-out p piano)        
        ]
    (>>>
     p
     p
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

        ;;_
        ;;(try-out intro drum-pattern)
        ]
    
    (>>> 
     intro
     first-sec
     intro-repeat
     first-sec
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
               ^inst=violin-marc bes4*104 
              " :relative :c5 :instruments instruments)

        
        ]
    (>>>
     (rest-for 32)
     part-b-first-time
     part-a-second-time-round
     part-a-second-time-round
     part-b-first-time
     )
    )
  )

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
     )
    (-> drum-pattern (on-inst :drum-set))
    (-> bass (on-inst :syn-bass))
    (-> bell-one (on-inst :muted-bell))
    violin
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
         ;;:beat-zero 32
         ;;:beat-zero 30
         ;;:beat-zero 14
         ;;:beat-end 33
         :beat-clock clock
         ))))

