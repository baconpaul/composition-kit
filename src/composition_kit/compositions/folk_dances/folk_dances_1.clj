(ns composition-kit.compositions.folk-dances.folk-dances-1
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

;; TO DO
;;   Dynamics on the synth lead
;;   Add Bells to third synth

(def synth-lead (midi/midi-instrument 0))
(def piano (midi/midi-instrument 1))
(def synth-bass (midi/midi-instrument 2))
(def marimba (midi/midi-instrument 3))
(def bells (midi/midi-instrument 4))

(def clock (tempo/constant-tempo 5 8 130))

(defn rstr [n s] (str (apply str (interpose " " (repeat n s))) " "))


;;;;;; FIRST SECTION

(def first-lead
  (let [theme-a    (->
                    (lily
                     (str
                      (rstr 2 "bes'4. bes4 bes16 aes g8 f c ees")
                      (rstr 2 "ees'4. ees4 ees16 des c8 bes ges aes")
                      "bes4. bes4 bes16 aes g8 f c ees "
                      "bes'4. bes4 bes16 aes g8 f c ees "
                      (rstr 2 "ees'4. ees4 ees16 des c8 bes ges aes")))
                    ;; make those notes stacatto when...
                    (ls/transform-note-payload
                     (fn [i p] (if (>= (:dur p) 1)
                                 (assoc p :hold-for 0.99)
                                 (assoc p :hold-for 0.1)))))

        theme-b     (-> (lily "bes8 aes g16 f bes4
                               ees,8 f g16 ees aes4
                               bes8 aes g16 f bes4
                               ees,8 f g16 ees aes4
                               ees'8 des c16 bes aes8 g
                               c8 bes aes16 g d8 f" :relative :c5)
                        (ls/line-segment-dynamics 0  90
                                                  2.4 80
                                                  2.5 100
                                                  4.9 80)
                        (ls/hold-for-pct 0.3)
                        )

        theme-b-alt  (>>>
                      (-> (lily "bes8 aes g16 f ees8 d
	                         g f ees16 d c8 bes

                              	 bes'8 aes g16 f ees8 d
	                         g f ees16 d c8 bes
	                         ees d c16 bes f8 bes
	                         g8 a bes16 c d8 fis 
                                 g8 r8"
                                :relative :c5)
                          (ls/hold-for-pct 0.3))
                      (-> (lily "a'16 bes c d ees8 f g a" :relative :c5)
                          (ls/hold-for-pct 0.3)
                          (ls/line-segment-dynamics 0 40 2.5 70)
                          )
                      )
        
        ]
    (>>>
     theme-a
     theme-b
     theme-a
     theme-b-alt
     )
    )
  )


(def first-piano-mid
  (let [chord-dyn #(ls/line-segment-dynamics % 0   70
                                             3/2 60
                                             5/2 80
                                             5   70
                                             15/2 65)
        chord-ped (ls/pedal-held-and-cleared-at 0 5 9.5)

        chord-ees   (ls/loop-n (lily "<ees c' des>4. <ees c' des>4" :relative :c3) 4)
        chord-aes   (ls/loop-n (lily "<aes f' ges>4. <aes f' ges>4" :relative :c4) 4)

        phrase-a    (ls/loop-n (>>>
                                (<*>
                                 (-> chord-ees chord-dyn)
                                 chord-ped
                                 )
                                
                                (<*>
                                 (-> chord-aes chord-dyn)
                                 chord-ped)
                                )
                               2)

        phrase-a-alt-bass
        (>>>
         (->
          (lily "c8 des ees f g aes bes c des ees ees4. ees,4 ees'4. ees,4" :relative :c3)
          (ls/line-segment-dynamics 0 70 10/2 90 15/2 80))
         (->
          (lily "f8 ges aes bes c des ees f ges aes aes4. aes,4 aes'4. aes,4" :relative :c3)
          (ls/line-segment-dynamics 0 60 10/2 90 15/2 80)))

        ]
    (>>>
     phrase-a
     
     (ls/loop-n
      (->
       (lily (rstr 5 "<ees f bes>8"))
       (ls/explicit-dynamics 70 62 54 71 61)) 6)
     
     (<*>
      phrase-a
      (ls/loop-n phrase-a-alt-bass 2))

     (ls/loop-n
      (->
       (lily (rstr 5 "<ees f bes>8"))
       (ls/explicit-dynamics 70 62 55 72 62)) 5)

     (<*>
      (-> 
       (lily (rstr 5 "<d g bes>8"))
       (ls/explicit-dynamics 72 68 74 82 78)
       (ls/loop-n 5)) 
      (-> 
       (lily "g8 a bes16 c d8 fis g4. g,4 g'4. fis,4 g'4. g,4 g'4. fis4" :relative :c3)
       (ls/line-segment-dynamics 0 20 2.5 80))
      )
     )
    )
  )



(def first-bass
  (let [bass-a (>>>
                (->
                 (lily (rstr 4 "ees4. ees4") (rstr 4 "aes4. aes4") :relative :c2)
                 (ls/line-segment-dynamics 0 10 15/2 40 30/2 80))
                (->
                 (lily  "c8 des d ees4 ees4. ees4" :relative :c2)
                 (ls/line-segment-dynamics 0 80 3/2 120 1.6 80)
                 (ls/loop-n 2))
                (-> (lily  "f8 ges g aes4 aes4. aes4" :relative :c2)
                    (ls/line-segment-dynamics 0 80 3/2 120 1.6 80)
                    (ls/loop-n 2))
                )
        
        bass-b   (>>>
                  (-> 
                   (lily "bes8 aes g16 f bes4
                          ees,8 f g16 ees aes4
                          bes8 aes g16 f bes4
                          ees,8 f g16 ees aes4
      
                          ees'8 des c16 bes aes8 g
                          c8 bes aes16 g d8 f" :relative :c3)
                   (ls/hold-for-pct 0.3)
                   ))
        bass-a-alt (->
                    (>>>
                     (-> (lily "c8 des ees f g aes bes c des ees ees,4. ees'4 ees,4. ees'4" :relative :c2)
                         (ls/line-segment-dynamics 0 60 10/2 90))
                     (->
                      (lily "f8 ges aes bes c des ees f ges aes aes,4. aes'4 aes,4. aes'4" :relative :c2)
                      (ls/line-segment-dynamics 0 60 10/2 90)
                      ))
                    (ls/loop-n 2))
        bass-b-alt  (->(lily "bes8 aes g16 f ees8 d
	                        g f ees16 d c8 bes

                            	bes'8 aes g16 f ees8 d
	                        g f ees16 d c8 bes
	                        ees d c16 bes f8 bes
	                        g8 a bes16 c d8 fis
                                g4. g,4 g4. fis'4 g4. g,4 g4. fis4"
                             :relative :c3)
                       (ls/hold-for-pct 0.5)
                       )
        ;;_ (-> bass-b-alt (on-instrument synth-bass) (with-clock clock) (midi-play))
        ]
    (>>>
     bass-a
     bass-b
     bass-a-alt
     bass-b-alt)
    )
  )


(defn mar-arp-pat [pat & notes]
  "make that marimba arpeggio from the three note pattern, starting at octave 3"
  (let [note-map (apply hash-map (flatten (map-indexed vector notes)))
        arp  pat
        ]
    (->
     (ls/sequence-from-pitches-and-durations
      (map #(keyword (str (name (get note-map (first %))) (inc (second %)))) arp)
      (repeat (count arp) 1/4))
     (ls/explicit-dynamics 87 60 62 63 65 64 82 72 65 62)))
  )

(defn mar-arp [& notes]
  (apply (partial mar-arp-pat [ [ 0 3 ] [ 1 3 ] [ 2 3 ] [ 1 4 ] [ 0 3 ] [ 2 3 ] [ 0 4 ] [ 1 3 ] [ 2 4 ] [ 0 4 ] ]) notes))


(def first-marimba
  (>>>
   (ls/loop-n (mar-arp :ees :c :des) 4)
   (ls/loop-n (mar-arp :aes :f :ges) 4)
   (ls/loop-n (mar-arp :ees :c :des) 4)
   (ls/loop-n (mar-arp :aes :f :ges) 4)

   (ls/loop-n (mar-arp :ees :f :bes) 6)

   (ls/loop-n (mar-arp :c :ees :des) 4)
   (ls/loop-n (mar-arp :f :aes :ges) 4)
   (ls/loop-n (mar-arp :des :c :ees) 4)
   (ls/loop-n (mar-arp :ges :f :aes) 4)

   (ls/loop-n (mar-arp :ees :f :bes) 3)
   (ls/loop-n (mar-arp :f :ees :bes) 2)
   (ls/loop-n (mar-arp :g :d :bes) 1)

   (ls/loop-n (mar-arp :g :d :bes) 2)
   (ls/loop-n (mar-arp :d :g :bes) 2)

   )
  )

;;;; SECOND SECTION

(def second-piano-mid
  (let [phrase-alt  (<*>
                     (->
                      (lily " <d g bes>4. <d fis bes>4" :relative :c4)
                      (ls/explicit-dynamics 70 64)
                      (ls/loop-n 8))
                     (ls/pedal-held-and-cleared-at 0 20))
        phrase-two (lily "<g bes d>4. <g bes ees>4
                   <g bes d>4 <g bes ees>8 <fis a d>4
                   <g bes d>4. <g bes ees>4
                   <g bes d>4 <fis a d>8 <g bes d>4")]
    (>>>
     phrase-alt
     phrase-two
     )
    )
  )

(defn mar-arp-b [& notes]
  (apply (partial mar-arp-pat [ [ 0 2 ] [ 1 2 ] [ 2 3 ] [ 1 2 ] [ 0 3 ] [ 2 4 ] [ 0 3 ] [ 3 3 ] [ 2 3 ] [ 0 2 ] ]) notes))

(defn mar-flam [dur & notes]
  (->  (ls/concrete-logical-sequence
        (map (fn [n idx] (i/notes-with-duration n (- dur (* idx 0.07)) (* idx 0.07)))
             notes
             (range (count notes))))
       (ls/hold-for-pct 0.7)
       (ls/line-segment-dynamics 0 70 (* (count notes) 0.08) 100)
       ))

(def second-marimba
  (>>>
   (ls/loop-n (mar-arp-b :d :g :bes :fis) 8)
   (mar-arp :d :g :bes)

   (>>>
    (mar-flam 3/2 :d4 :g4 :bes4)
    (mar-flam 1   :fis4 :bes4 :d5)
    )

   (mar-arp :d :g :bes)

   (>>>
    (mar-flam 1 :d4 :g4 :bes4)
    (mar-flam 1/2   :fis4 :bes4 :d5)
    (mar-flam 1 :d4 :g4 :bes4)
    )
   )
  )

(def second-bass
  (-> (ls/sequence-from-pitches-and-durations
       [[ :g2 :g1 ]
        [ :g2 :g1 ] [ :fis2 :fis1 ]
        [ :g2 :g1 ] [ :fis2 :fis1 ] [ :g2 :g1 ]]
       [ 20 4 1 7/2 1/2 1])
      (ls/hold-for-pct 0.9999))
  )


(def bell-lead
  (let [lp   (->
              (ls/loop-n
               (lily "bes''8 a g fis d
                      bes'8 a g fis d
                      c d ees a,4
                      c4. d4" :relative :c3) 2)
              (ls/line-segment-dynamics 0 80 2.45 70 2.5 85 4.95 65
                                        5 83 7.5 84 9.85 60
                                        10 83 15 80 16 92 20 72
                                        )
              )
        sp  (lily "d4. ees4 bes8 c ees d4 d4. ees4 bes16 a g8 fis g4" :relative :c4)
        ]
    ;; this is messier than it needs to be
    (>>> 
     (-> lp
         (as-> ls
             (<*>
              ls
              (-> ls
                  (ls/amplify 0.2)
                  (ls/transpose 7)
                  (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.125)))
                  )
              (-> ls
                  (ls/amplify 0.4)
                  (ls/transpose 12)
                  (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.25)))
                  )
              )))
     (-> sp
         (as-> ls
             (<*>
              ls
              (-> ls
                  (ls/amplify 0.2)
                  (ls/transpose 7)
                  (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.125)))
                  )
              (-> ls
                  (ls/amplify 0.4)
                  (ls/transpose 12)
                  (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.25)))))))
     
     )
    )
  )

(def third-piano
  "This is the big leaping eflat aflat bflat combo"
  (let [lh-p124
        (->
         (lily "<ees ees'>8 <g' bes ees> <g, g'> <aes aes> <c' ees bes>
                 <bes, bes'>8 <f' bes c> <f, f'> <ees ees'> <g' ees bes>" :relative :c2)
         (ls/hold-for-pct 0.7)
         (ls/explicit-segment-dynamics '(90 70 85 92 72
                                            92 70 84 87 73))
         )

        lh-p124-end
        (->
         (lily "<ees ees'>8 <g' bes ees> <g, g'> <aes aes> <c' ees bes>
                 <bes, bes'>8 <f' bes c> <f, f'> <ees ees'>8 ees" :relative :c2)
         (ls/hold-for-pct 0.7)
         (ls/explicit-segment-dynamics '(90 70 85 92 72
                                            92 70 84 92 101))
         )
        lh-p3
        (->
         (lily "<ees ees'>8 <g' bes ees> <g, g'> <aes aes> <c' ees bes>
                 <bes, bes'>8 <f' bes c> <b, b'> <c c'> <g' c ees>" :relative :c2)
         (ls/hold-for-pct 0.7)
         (ls/explicit-segment-dynamics '(90 70 85 92 72
                                            92 70 84 87 73))
         )
        lh-p3-alt
        (->
         (lily "<ees ees'>8 <g' bes ees> <g, g'> <aes aes> <c' ees bes>
                 <bes, bes'>8 <f' bes c> <d d'> <c c'> <g' c ees>" :relative :c2)
         (ls/hold-for-pct 0.7)
         (ls/explicit-segment-dynamics '(90 70 85 92 72
                                            92 70 84 87 73))
         )

        rh-p124
        (->
         (lily "<bes ees f>8 <ees f g> r4 <g aes d>8
                <bes, d f> <bes c f> r4."
               :relative :c5)
         (ls/hold-for-pct 0.4)
         (ls/explicit-segment-dynamics '(90 70 0 80 82 70 0)) ;; remember the rests are in explicit
         )

        rh-p124-end
        (->
         (lily "<bes ees f>8 <ees f g> r4 <g aes d>8
                <bes, d f> <bes c f> r8 <ees g bes>8 <ees aes bes>"
               :relative :c5)
         (ls/hold-for-pct 0.4)
         (ls/explicit-segment-dynamics '(90 70 0 80 82 70 0 98 103)) ;; remember the rests are in explicit
         )

        rh-p3
        (->
         (lily "<bes ees f>8 <ees f g> r4 <g aes d>8
                <bes, d f> <c d f> r4."
               :relative :c5)
         (ls/hold-for-pct 0.4)
         (ls/explicit-segment-dynamics '(90 70 0 80 82 70 0)) ;; remember the rests are in explicit
         )

        rh-p3-alt
        (->
         (lily "<bes ees f>8 <ees f g> r4 <g aes d>8
                <f f'>8 <fis f'> <g f'> g'16 f g8"
               :relative :c5)
         (ls/hold-for-pct 0.4)
         (ls/explicit-segment-dynamics '(90 70 0 80 82 88 88 88 92 87 92)) ;; remember the rests are in explicit
         )

        ]
    (<*>
     (>>>
      lh-p124
      lh-p124
      lh-p3
      lh-p124
      
      lh-p124
      lh-p124
      lh-p3-alt
      lh-p124-end

      )
     (>>>
      rh-p124
      rh-p124
      rh-p3
      rh-p124

      rh-p124
      rh-p124
      rh-p3-alt
      rh-p124-end
      )
     )
    ))

(def third-bass
  (let [ph (->
            (lily "ees4 g8 aes4 bes4 f8 ees4
                     ees4 g8 aes4 bes4 d,8 ees4
                     ees4 g8 aes4 bes4 b8 c4
                     ees,4 g8 aes4 bes4 f8 ees4

                     ees4 g8 aes4 bes4 f8 ees4
                     ees4 g8 aes4 bes4 d,8 ees4
                     ees4 g8 aes4 bes4 d8 c4
                     ees,4 g8 aes4 bes4 f8 ees8 ees'
"
                  :relative :c2)
            (ls/hold-for-pct 0.99)
            (ls/line-segment-dynamics 0 75 5/2 88 5 77 15/2 88 10 77 14.99 94 
                                      15 75 35/2 88 20 77 45/2 88 30 77 35 94 )
            )
        ]
    (<*> ph (ls/transpose ph -12))
    ))

(def third-marimba
  (let [p-124 (lily "ees32 d ees d ees16 f g g, g' g, aes8
                       r4. r4
                       ees'32 d ees d ees16 f g g, g' g, aes8
                       r4. r4
                       bes32 a bes a bes16 f b f c des c'8
                       r4. r4
                       ees,32 d ees d ees16 f g g, g' g, aes8
                       r4. r4
                      " :relative :c5 )
        p-end (->
               (lily "<ees ees'>16 <ees ees'> <ees ees'>8 " :relative :c5)
               (ls/amplify 1.3)
               (ls/hold-for-pct 0.2)
               )
        ]
    (>>> p-124 p-124 p-end)
    ))

                                        ;(do
(def third-lead
  (let [p-1 (->
             (lily "g4. bes4 aes16 g f8 d ees4
                    g4. bes4 aes16 g f8 d ees4
                    g4. bes4 aes16 g f8 d' c4
                    g4. bes4 aes16 g f8 d ees4" :relative :c5)
             (ls/line-segment-dynamics 0 90 5 92 10 94 15 92)
             (ls/transform-note-payload
              (fn [i p] (if (>= (:dur p) 1)
                          (assoc p :hold-for 0.99)
                          (assoc p :hold-for 0.1)))))
        ]
    (>>>
     p-1
     (<*>
      p-1
      (->
       p-1
       (ls/transpose 12)
       (ls/amplify 0.6)
       )
      ))
    )
  )

(def third-bell
  (let [p-1 (->
             (lily "g4. bes4 r4. r4" :relative :c5)
             (ls/loop-sequence 8))
        p-ech (<*>
               p-1
               (-> p-1
                    (ls/amplify 0.2)
                    (ls/transpose 7)
                    (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.125)))
                    )
               (-> p-1
                    (ls/amplify 0.4)
                    (ls/transpose 12)
                    (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.25)))))
        p-end-r (->
                 (lily "ees16 ees' ees,8" :relative :c5)
                 (ls/explicit-segment-dynamics '(60 70 60)))
        p-end (<*>
               p-end-r
               (-> p-end-r
                    (ls/amplify 0.2)
                    (ls/transpose 7)
                    (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.125)))
                    )
               (-> p-end-r
                    (ls/amplify 0.4)
                    (ls/transpose 12)
                    (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.25)))))
        ;;_ (try-out p-ech bells)
        ]
    (>>> p-1 p-end)))


(def fourth-lead
  (let [theme-a    (->
                    (lily
                     (str
                      (rstr 2 "bes'4. bes4 bes16 aes g8 f c ees")
                      (rstr 2 "ees'4. ees4 ees16 des c8 bes ges aes")
                      "bes4. bes4 bes16 aes g8 f c ees "
                      "bes'4. bes4 bes16 aes g8 f c ees "
                      (rstr 2 "ees'4. ees4 ees16 des c8 bes ges aes")))

                    ;; make those notes stacatto when...
                    (ls/transform-note-payload
                     (fn [i p] (if (>= (:dur p) 1)
                                 (assoc p :hold-for 0.99)
                                 (assoc p :hold-for 0.1)))))

        theme-b     (>>>
                     (->
                      (<*> 
                       (lily " bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      
      ees'8 des c16 bes aes8 g
      c8 bes aes16 g f8 ees

      ees'8 des c16 bes aes8 g
      c8 bes aes16 g f8 ees

      ees'8 des c16 bes aes8 g
      c8 bes aes16 g f8 ees

      ees'8 des c16 bes aes8 g
      c8 bes aes16 g d8 f
" :relative :c5)
                       (>>> (rest-for 20)
                            (-> (lily "
                                 g8 f ees16 des c8 bes
                                 ees8 des c16 bes aes8 g

                                 g'8 f ees16 des c8 bes
                                 ees8 des c16 bes f8 aes
                                 " :relative :c5)
                                (ls/amplify 0.5)
                                )
                            )
                       
                       (>>> (rest-for 25)
                            (-> (lily "
                                 bes8 aes g16 f ees8 des
                                 g8 f ees16 des aes8 c
                                 " :relative :c5)
                                (ls/amplify 0.3)
                                )
                            )
                       )

                      (ls/hold-for-pct 0.3)
                      )
                     ;; I should have a syntax for in-chord dynamics but
                     (<*>
                      (-> (ls/sequence-from-pitches-and-durations [:ees5 ] [ 1/4 ]) (ls/explicit-segment-dynamics '(90)))
                      (-> (ls/sequence-from-pitches-and-durations [:bes4 ] [ 1/4 ]) (ls/explicit-segment-dynamics '(80)))
                      (-> (ls/sequence-from-pitches-and-durations [:g4 ] [ 1/4 ]) (ls/explicit-segment-dynamics '(70)))
                      (-> (ls/sequence-from-pitches-and-durations [:ees4 ] [ 1/4 ]) (ls/explicit-segment-dynamics '(75)))
                      )
                     )

        ]
    (>>>
     theme-a
     (-> theme-b
         (ls/line-segment-amplify 0 1 10 1.05 20 1.4))
     )
    )
  )

(def fourth-piano
  (let [rh-intr (lily "r4. r4 r4. <ees c' des>8 <ees c' des>" :relative :c4)
        rh-ahit (lily "r4. r4 r4. <aes f' ges>8 <aes f' ges>" :relative :c4)
        rh-ees  (->
                 (lily "r8 <ees c>16 <c des> <c ees>8 r4" :relative :c5)
                 (ls/explicit-segment-dynamics '(0 79 75 77 0))
                 (ls/hold-for-pct 0.6)
                 )
        rh-aes  (->
                 (lily "r8 <ees c>16 <c f> <c ees>8 r4" :relative :c5)
                 (ls/explicit-segment-dynamics '(0 79 75 77 0))
                 (ls/hold-for-pct 0.6)
                 )

        rh-ees-b  (->
                   (lily "<ees c>16 <c des> <c ees>8 r8 r4" :relative :c5)
                   (ls/explicit-segment-dynamics '(79 75 77 0 0))
                   (ls/hold-for-pct 0.6)
                   )
        rh-aes-b  (->
                   (lily "<ees c>16 <c f> <c ees>8 r8 r4" :relative :c5)
                   (ls/explicit-segment-dynamics '(79 75 77 0 0))
                   (ls/hold-for-pct 0.6)
                   )

        lh-intr (lily "r4. r4 r4. <ees ees'>8 <ees ees'>" :relative :c1)
        lh-ees  (->
                 (lily "<ees, ees'>8 <des' c'> <c'' des ees> <ees,, ees'> <ees, ees'>" :relative :c2)
                 (ls/explicit-segment-dynamics '(80 82 70 84 78))
                 )
        lh-aes  (->
                 (lily "<aes aes'>8 <ges' f'> <f'' ges aes> <aes,, aes'> <aes, aes'>" :relative :c2)
                 (ls/explicit-segment-dynamics '(80 82 70 84 78))
                 )

        ees-lh-scale (lily "c8 des ees f g aes bes c des ees" :relative :c2)
        lh-ees-b (>>>
                  (->
                   (<*> ees-lh-scale
                        (-> ees-lh-scale
                            (ls/transpose -12))
                        )
                   (ls/line-segment-dynamics 0 50 5 106))
                  lh-ees
                  lh-ees
                  )
        aes-lh-scale (lily "f8 ges aes bes c des ees f ges aes" :relative :c2)
        lh-aes-b (>>>
                  (->
                   (<*> aes-lh-scale
                        (-> aes-lh-scale
                            (ls/transpose -12)))
                   (ls/line-segment-dynamics 0 50 5 106)
                   )
                  lh-aes
                  lh-aes
                  )

        ;;_ (try-out lh-ees piano)
        ;;chord-ees   (ls/loop-n (lily "<ees c' des>4. <ees c' des>4" :relative :c3) 4)
        ;;chord-aes   (ls/loop-n (lily "<aes f' ges>4. <aes f' ges>4" :relative :c4) 4)

        end-chord-a
        (->
         (lily (rstr 5 "<ees f bes>8"))
         (ls/explicit-dynamics 70 62 54 71 61))


        end-chord-b
        (->
         (lily "<ees f bes>8 <ees f bes>8 <ees g bes>8 <ees f bes>8 <ees g bes>8" )
         (ls/explicit-dynamics 70 62 54 71 61))

        end-chord-c
        (->
         (lily "<ees f bes>8 <ees f c'>8 <ees g c'>8 <ees f c>8 <ees g bes>8" )
         (ls/explicit-dynamics 70 62 54 71 61))

        matched-phrase
        (->
         (<*>
          (-> (lily "ees'8 des c16 bes aes8 g
                       c8 bes aes16 g d8 f
                                 " :relative :c5)
              (ls/amplify 1.1)
              )
          (-> (lily "g'8 f ees16 des c8 bes
                        ees8 des c16 bes f8 aes
                                 " :relative :c5)
              (ls/amplify 1.1)
              )
          
          (-> (lily "bes8 aes g16 f ees8 des
                        g8 f ees16 des aes8 c"
                    :relative :c5)
              (ls/amplify 1.13))))

        lh-end
        (>>>
         (->
          (<*>
           (ls/loop-n (lily "ees4. ees4" :relative :c1) 8)
           (ls/loop-n (lily "ees4. ees4" :relative :c2) 8))
          (ls/hold-for-pct 0.99)
          (ls/line-segment-dynamics 0 70 20 100))
         (-> (ls/sequence-from-pitches-and-durations [ [ :ees2 :ees3 ] ] [ 1 ]) (ls/explicit-dynamics 102))
         )

        res
        (<*>
         (>>>
          rh-intr
          (ls/loop-n rh-ees 2)
          (ls/loop-n rh-aes 4)
          rh-intr
          (ls/loop-n rh-ees-b 2)
          rh-ahit
          (ls/loop-n rh-aes-b 2)

          end-chord-a end-chord-b end-chord-a end-chord-b

          (-> end-chord-c (ls/amplify 1.02))
          (-> end-chord-b (ls/amplify 1.04))
          (-> end-chord-c (ls/amplify 1.06))
          (-> end-chord-a (ls/amplify 1.08))

          (-> end-chord-c (ls/amplify 1.1))
          (-> end-chord-b (ls/amplify 1.13))
          (-> end-chord-c (ls/amplify 1.16))
          (-> end-chord-a (ls/amplify 1.18))

          
          (-> (lily "<ees g bes ees>4" :relative :c4))
          )
         (>>>
          lh-intr
          (ls/loop-n lh-ees 2)
          (ls/loop-n lh-aes 4)
          lh-ees-b
          lh-aes-b
          (rest-for 10)
          lh-end
          )
         )

        ;;_ (try-out lh-aes-b piano)
        ]

    res
    ))

(def fourth-bass ;; surely you mean home plate?
  (let [p-1 (lily "r4. r4 r4. ees8 ees," :relative :c2)
        p-e (lily "r4. r4 r4. r8 ees," :relative :c2)
        p-a (lily "r4. r4 r4. r8 aes," :relative :c2)
        p-ees (lily "ees4. ees4" :relative :c2)
        p-ees-o (<*> p-ees
                     (-> p-ees (ls/transpose -12)))
        p-aes (lily "aes4. aes4" :relative :c2)
        p-aes-o (<*> p-aes
                     (-> p-aes (ls/transpose -12)))

        lh-end
        (>>>
         (->
          (<*>
           (ls/loop-n (lily "ees4. ees4" :relative :c1) 8)
           (ls/loop-n (lily "ees4. ees4" :relative :c2) 8))
          (ls/hold-for-pct 0.99)
          (ls/line-segment-dynamics 0 70 20 100))
         (-> (ls/sequence-from-pitches-and-durations [ [ :ees2 :ees3 ] ] [ 1 ] ) (ls/explicit-dynamics 102)))


        res
        (>>>
         p-1
         (->
          (>>>
           (ls/loop-n p-ees-o 2)
           (ls/loop-n p-aes-o 4)
           p-e
           (ls/loop-n p-ees-o 2)
           p-a
           (ls/loop-n p-aes-o 2)
           )
          (ls/hold-for-pct 0.999))
         (rest-for 10)
         lh-end
         )
        ;;_ (try-out res synth-bass)
        ]
    res
    ))

(defn five-beat-oct-creschendo [n1 n2]
  (-> (ls/sequence-from-pitches-and-durations
       [n1 n2] [1/4 1/4])
      (ls/loop-sequence 10)
      (ls/line-segment-dynamics 0 40 10 100)))

(def fourth-mar
  (let [p-1 (lily "r4. r4 r4. ees16 bes' <bes ees> <ees g>" :relative :c4)
        ]
    (>>>
     p-1
     (ls/loop-n (mar-arp :ees :c :des) 2)
     (ls/loop-n (mar-arp :aes :f :ges) 4)

     (five-beat-oct-creschendo :ees4 :ees5)
     (ls/loop-n (mar-arp :ees :c :des) 2)
     (five-beat-oct-creschendo :aes4 :aes5)
     (ls/loop-n (mar-arp :aes :f :ges) 2)
     (rest-for 10)

     (->
      (>>>
       (ls/loop-n (mar-arp :ees :f :bes) 2)
       (ls/loop-n (mar-arp-b :ees :f :bes :c) 2)
       (ls/loop-n (mar-arp :ees :f :bes) 2)
       (ls/loop-n (mar-arp-b :ees :f :bes :c) 2)
       )
      (ls/line-segment-amplify 0 1.0 20 1.3)
      )
     )
    ))


(def final-song
  (-> (>>>
       (<*>
        (-> first-lead (ls/on-instrument synth-lead))
        (-> first-piano-mid (ls/on-instrument piano))
        (-> first-marimba (ls/on-instrument marimba))
        (-> first-bass (ls/on-instrument synth-bass))
        )
       (<*>
        (-> second-piano-mid (ls/on-instrument piano))
        (-> bell-lead (ls/on-instrument bells))
        (-> second-marimba (ls/on-instrument marimba))
        (-> second-bass (ls/on-instrument synth-bass))
        )
       (<*>
        (-> third-piano (ls/on-instrument piano))
        (-> third-bass (ls/on-instrument synth-bass))
        (-> third-marimba (ls/on-instrument marimba))
        (-> third-lead (ls/on-instrument synth-lead))
        (-> third-bell (ls/on-instrument bells))
        )
       (<*>
        (-> fourth-lead (ls/on-instrument synth-lead))
        (-> fourth-piano (ls/on-instrument piano))
        (-> fourth-bass (ls/on-instrument synth-bass))
        (-> fourth-mar (ls/on-instrument marimba))
        )
       )
      
      (ls/with-clock clock) 
      ))

(defn try-out [p i]
  (-> p (ls/on-instrument i) (ls/with-clock clock) (midi-play)))


(def play-it true)
(def player
  (when play-it
    (midi-play
     final-song
     
     :beat-zero 100
     :beat-end 180

     :beat-clock clock
     ))
  ;; 120 is second bit; 150 is third bit
  )

(agent-errors player)

;;(try-out first-lead synth-lead)

;;(def sss (composition-kit.events.physical-sequence/stop player))


