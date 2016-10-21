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
  (let [theme-a    (-*>
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

        theme-b     (-*> (phrase
                          (lily " bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      
      ees'8 des c16 bes aes8 g
      c8 bes aes16 g d8 f" :relative :c5)
                          (dynamics-at 0 -> 90 2.4 -> 80
                                       2.5 -> 100 4.9 -> 80))
                         (ls/hold-for-pct 0.3)
                         )

        theme-b-alt  (>>>
                      (-*> (phrase
                            (lily "bes8 aes g16 f ees8 d
	                        g f ees16 d c8 bes

                            	bes'8 aes g16 f ees8 d
	                        g f ees16 d c8 bes
	                        ees d c16 bes f8 bes
	                        g8 a bes16 c d8 fis"
                                  :relative :c5))
                           (ls/hold-for-pct 0.3))
                      (phrase (pitches :g4) (durations 5))) ;; to keep it out of the hold
        
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
  (let [chord-dyn (dynamics-at 0 -> 70 3/2 -> 60 5/2 -> 80 5 -> 70 15/2 -> 65)
        chord-ped (pedal-held-and-cleared-at 0 5 9.5)

        chord-ees   (loop-n (lily "<ees c' des>4. <ees c' des>4" :relative :c3) 4)
        chord-aes   (loop-n (lily "<aes f' ges>4. <aes f' ges>4" :relative :c4) 4)

        phrase-a    (loop-n (>>> 
                             (phrase chord-ees chord-dyn chord-ped)
                             (phrase chord-aes chord-dyn chord-ped)
                             )
                            2)

        phrase-a-alt-bass
        (>>>
         (phrase
          (lily "c8 des ees f g aes bes c des ees ees4. ees,4 ees'4. ees,4" :relative :c3)
          (dynamics-at 0 -> 70 10/2 -> 90 15/2 -> 80))
         (phrase
          (lily "f8 ges aes bes c des ees f ges aes aes4. aes,4 aes'4. aes,4" :relative :c3)
          (dynamics-at 0 -> 60 10/2 -> 90 15/2 -> 80)))

        ]
    (>>>
     phrase-a
     
     (loop-n
      (phrase
       (lily (rstr 5 "<ees f bes>8"))
       (dynamics 70 62 54 71 61)) 6)
     
     (<*>
      phrase-a
      (loop-n phrase-a-alt-bass 2))

     (loop-n
      (phrase
       (lily (rstr 5 "<ees f bes>8"))
       (dynamics 70 62 55 72 62)) 5)

     (<*>
      (-> (phrase
           (lily (rstr 5 "<d g bes>8"))
           (dynamics 72 68 74 82 78))
          (loop-n 5)) ;; 1 + 4
      (-> (phrase
           (lily "g8 a bes16 c d8 fis g4. g,4 g'4. fis,4 g'4. g,4 g'4. fis4" :relative :c3)
           (dynamics-at 0 -> 20 2.5 -> 80))
          ;;(lift-to-seq ls/<*>-octave-below))
          )
      )
     )
    )
  ) 



(def first-bass
  (let [bass-a (>>>
                (phrase
                 (lily (rstr 4 "ees4. ees4") (rstr 4 "aes4. aes4") :relative :c2)
                 (dynamics-at 0 -> 10 15/2 -> 40 30/2 -> 80))
                (loop-n (phrase
                         (lily  "c8 des d ees4 ees4. ees4" :relative :c2)
                         (dynamics-at 0 -> 80 3/2 -> 120 1.6 80)) 2)
                (loop-n (phrase
                         (lily  "f8 ges g aes4 aes4. aes4" :relative :c2)
                         (dynamics-at 0 -> 80 3/2 -> 120 1.6 80)) 2))
        bass-b   (>>>
                  (-*> (phrase
                        (lily "bes8 aes g16 f bes4
                          ees,8 f g16 ees aes4
                          bes8 aes g16 f bes4
                          ees,8 f g16 ees aes4
      
                          ees'8 des c16 bes aes8 g
                          c8 bes aes16 g d8 f" :relative :c3))
                       (ls/hold-for-pct 0.3)
                       ))
        bass-a-alt (loop-n (>>>
                            (phrase
                             (lily "c8 des ees f g aes bes c des ees ees,4. ees'4 ees,4. ees'4" :relative :c2)
                             (dynamics-at 0 -> 60 10/2 -> 90)
                             
                             )
                            (phrase
                             (lily "f8 ges aes bes c des ees f ges aes aes,4. aes'4 aes,4. aes'4" :relative :c2)
                             (dynamics-at 0 -> 60 10/2 -> 90)
                             ) ) 2)
        bass-b-alt  (-*>(lily "bes8 aes g16 f ees8 d
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
    (phrase 
     (apply pitches (map #(keyword (str (name (get note-map (first %))) (inc (second %)))) arp))
     (apply durations (repeat (count arp) 1/4))
     (dynamics 87 60 62 63 65 64 82 72 65 62)))
  )

(defn mar-arp [& notes]
  (apply (partial mar-arp-pat [ [ 0 3 ] [ 1 3 ] [ 2 3 ] [ 1 4 ] [ 0 3 ] [ 2 3 ] [ 0 4 ] [ 1 3 ] [ 2 4 ] [ 0 4 ] ]) notes))


(def first-marimba
  (>>>
   (loop-n (mar-arp :ees :c :des) 4)
   (loop-n (mar-arp :aes :f :ges) 4)
   (loop-n (mar-arp :ees :c :des) 4)
   (loop-n (mar-arp :aes :f :ges) 4)

   (loop-n (mar-arp :ees :f :bes) 6)

   (loop-n (mar-arp :c :ees :des) 4)
   (loop-n (mar-arp :f :aes :ges) 4)
   (loop-n (mar-arp :des :c :ees) 4)
   (loop-n (mar-arp :ges :f :aes) 4)

   (loop-n (mar-arp :ees :f :bes) 3)
   (loop-n (mar-arp :f :ees :bes) 2)
   (loop-n (mar-arp :g :d :bes) 1)

   (loop-n (mar-arp :g :d :bes) 2)
   (loop-n (mar-arp :d :g :bes) 2)

   )
  )

;;;; SECOND SECTION

(def second-piano-mid
  (let [phrase-alt  (<*>
                     (->
                      (phrase (lily " <d g bes>4. <d fis bes>4" :relative :c4)
                              (dynamics 70 64))
                      (loop-n 8))
                     (pedal-held-and-cleared-at 0 20))
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
  (-*> (raw-sequence (ls/concrete-logical-sequence
                      (map (fn [n idx] (i/notes-with-duration n (- dur (* idx 0.07)) (* idx 0.07)))
                           notes
                           (range (count notes)))))
       (ls/hold-for-pct 0.7)
       (ls/line-segment-dynamics 0 70 (* (count notes) 0.08) 100)
       ))

(def second-marimba
  (>>>
   (loop-n (mar-arp-b :d :g :bes :fis) 8)
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
  (-*> (>>> 
        (phrase (pitches [ :g2 :g1 ]) (durations 20))
        (phrase (pitches [ :g2 :g1 ] [ :fis2 :fis1 ] ) (durations 4 1))
        (phrase (pitches [ :g2 :g1 ] [ :fis2 :fis1 ] [ :g2 :g1 ] ) (durations 7/2 1/2 1)))
       (ls/hold-for-pct 0.9999))

  )


(def bell-lead
  (let [lp   (phrase
              (loop-n
               (lily "bes''8 a g fis d
                      bes'8 a g fis d
                      c d ees a,4
                      c4. d4" :relative :c3) 2)
              (dynamics-at 0 -> 80 2.45 -> 70 2.5 -> 85 4.95 -> 65
                           5 -> 90 7.5 -> 90 9.85 -> 60
                           10 -> 90 15 -> 80 16 -> 92 20 -> 60
                           )
              )
        sp (phrase
            (lily "d4. ees4 bes8 c ees d4 d4. ees4 bes16 a g8 fis g4" :relative :c4))
        ]
    ;; this is messier than it needs to be
    (>>> 
     (-> lp
         (as-> ls
             (<*>
              ls
              (-*> ls
                   (ls/amplify 0.2)
                   (ls/transpose 7)
                   (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.125)))
                   )
              (-*> ls
                   (ls/amplify 0.4)
                   (ls/transpose 12)
                   (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.25)))
                   )
              )))
     (-> sp
         (as-> ls
             (<*>
              ls
              (-*> ls
                   (ls/amplify 0.2)
                   (ls/transpose 7)
                   (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.125)))
                   )
              (-*> ls
                   (ls/amplify 0.4)
                   (ls/transpose 12)
                   (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.25)))))))
     
     )
    )
  )

(def third-piano
  "This is the big leaping eflat aflat bflat combo"
  (let [lh-p124
        (-*>
         (lily "<ees ees'>8 <g' bes ees> <g, g'> <aes aes> <c' ees bes>
                 <bes, bes'>8 <f' bes c> <f, f'> <ees ees'> <g' ees bes>" :relative :c2)
         (ls/hold-for-pct 0.7)
         (ls/explicit-segment-dynamics '(90 70 85 92 72
                                            92 70 84 87 73))
         )

        lh-p124-end
        (-*>
         (lily "<ees ees'>8 <g' bes ees> <g, g'> <aes aes> <c' ees bes>
                 <bes, bes'>8 <f' bes c> <f, f'> <ees ees'>8 ees" :relative :c2)
         (ls/hold-for-pct 0.7)
         (ls/explicit-segment-dynamics '(90 70 85 92 72
                                            92 70 84 92 101))
         )
        lh-p3
        (-*>
         (lily "<ees ees'>8 <g' bes ees> <g, g'> <aes aes> <c' ees bes>
                 <bes, bes'>8 <f' bes c> <b, b'> <c c'> <g' c ees>" :relative :c2)
         (ls/hold-for-pct 0.7)
         (ls/explicit-segment-dynamics '(90 70 85 92 72
                                            92 70 84 87 73))
         )
        lh-p3-alt
        (-*>
         (lily "<ees ees'>8 <g' bes ees> <g, g'> <aes aes> <c' ees bes>
                 <bes, bes'>8 <f' bes c> <d d'> <c c'> <g' c ees>" :relative :c2)
         (ls/hold-for-pct 0.7)
         (ls/explicit-segment-dynamics '(90 70 85 92 72
                                            92 70 84 87 73))
         )

        rh-p124
        (-*>
         (lily "<bes ees f>8 <ees f g> r4 <g aes d>8
                <bes, d f> <bes c f> r4."
               :relative :c5)
         (ls/hold-for-pct 0.4)
         (ls/explicit-segment-dynamics '(90 70 0 80 82 70 0)) ;; remember the rests are in explicit
         )

        rh-p124-end
        (-*>
         (lily "<bes ees f>8 <ees f g> r4 <g aes d>8
                <bes, d f> <bes c f> r8 <ees g bes>8 <ees aes bes>"
               :relative :c5)
         (ls/hold-for-pct 0.4)
         (ls/explicit-segment-dynamics '(90 70 0 80 82 70 0 98 103)) ;; remember the rests are in explicit
         )

        rh-p3
        (-*>
         (lily "<bes ees f>8 <ees f g> r4 <g aes d>8
                <bes, d f> <c d f> r4."
               :relative :c5)
         (ls/hold-for-pct 0.4)
         (ls/explicit-segment-dynamics '(90 70 0 80 82 70 0)) ;; remember the rests are in explicit
         )

        rh-p3-alt
        (-*>
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
  (let [ph (-*>
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
    (<*> ph (lift-to-seq ph ls/transpose -12))
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
                      " :relative :c5 )]
    (>>> p-124 p-124)
    ))

                                        ;(do
(def third-lead
  (let [p-1 (-*>
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
      (-*>
       p-1
       (ls/transpose 12)
       (ls/amplify 0.6)
       )
      ))
    )
  )

(def third-bell
  (let [p-1 (-*>
             (lily "g4. bes4 r4. r4" :relative :c5)
             (ls/loop-sequence 8))
        p-ech (<*>
               p-1
               (-*> p-1
                    (ls/amplify 0.2)
                    (ls/transpose 7)
                    (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.125)))
                    )
               (-*> p-1
                    (ls/amplify 0.4)
                    (ls/transpose 12)
                    (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.25)))))
        ;;_ (try-out p-ech bells)
        ]
    p-1))

(def final-song
  (-> (>>>
       (<*>
        (-> first-lead
            (on-instrument synth-lead))
        (-> first-piano-mid
            (on-instrument piano))
        (-> first-marimba
            (on-instrument marimba))
        (-> first-bass
            (on-instrument synth-bass))
        )
       (<*>
        (-> second-piano-mid (on-instrument piano))
        (-> bell-lead (on-instrument bells))
        (-> second-marimba (on-instrument marimba))
        (-> second-bass (on-instrument synth-bass))
        )
       (<*>
        (-> third-piano (on-instrument piano))
        (-> third-bass (on-instrument synth-bass))
        (-> third-marimba (on-instrument marimba))
        (-> third-lead (on-instrument synth-lead))
        (-> third-bell (on-instrument bells))
        )
       )
      
      (with-clock clock) 
      ))

(defn try-out [p i]
  (-> p (on-instrument i) (with-clock clock) (midi-play)))


;;(try-out third-piano piano)

(def play-it true)
(def player
  (when play-it
    (midi-play final-song :beat-zero 0))
  ;; 120 is second bit; 150 is third bit
  )



;;(def sss (composition-kit.events.physical-sequence/stop player))


