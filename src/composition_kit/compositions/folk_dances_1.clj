(ns composition-kit.compositions.folk-dances-1
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])

  (:use composition-kit.core))


(def synth-lead (midi/midi-instrument 0))
(def piano (midi/midi-instrument 1))
(def synth-bass (midi/midi-instrument 2))
(def marimba (midi/midi-instrument 3))

(def clock (tempo/constant-tempo 5 8 130))

(defn rstr [n s] (str (apply str (interpose " " (repeat n s))) " "))

(def first-lead
  (let [theme-a    (-> 
                    (str
                     (rstr 2 "bes'4. bes4 bes16 aes g8 f c ees")
                     (rstr 2 "ees'4. ees4 ees16 des c8 bes ges aes")
                     "bes4. bes4 bes16 aes g8 f c ees "
                     "bes'4. bes4 bes16 aes g8 f c ees "
                     (rstr 2 "ees'4. ees4 ees16 des c8 bes ges aes"))
                    lily
                    ;; make those notes stacatto when...
                    (transform-note-payload
                     (fn [i p] (if (>= (:dur p) 1)
                                 (assoc p :hold-for 0.99)
                                 (assoc p :hold-for 0.1)))))

        theme-b     (-> (phrase
                         (lily " bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      
      ees'8 des c16 bes aes8 g
      c8 bes aes16 g d8 f" :relative :c5)
                         (dynamics-at 0 -> 90 2.4 -> 80
                                      2.5 -> 100 4.9 -> 80))
                        (hold-for 0.15)
                        )

        theme-b-alt  (-> phrase
                         (lily "bes8 aes g16 f ees8 d
	                        g f ees16 d c8 bes

                            	bes'8 aes g16 f ees8 d
	                        g f ees16 d c8 bes
	                        ees d c16 bes f8 bes
	                        g8 a bes16 c d8 fis"
                               :relative :c5)
                         (hold-for 0.15))
        
        ]
    (concatenate
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

        phrase-a    (loop-n (concatenate 
                             (phrase chord-ees chord-dyn chord-ped)
                             (phrase chord-aes chord-dyn chord-ped)
                             )
                            2)

        phrase-a-alt-bass
        (concatenate
         (phrase
          (lily "c8 des ees f g aes bes c des ees ees4. ees,4 ees'4. ees,4" :relative :c3)
          (dynamics-at 0 -> 60 10/2 -> 110 15/2 -> 80))
         (phrase
          (lily "f8 g aes bes c des ees f ges aes aes4. aes,4 aes'4. aes,4" :relative :c3)
          (dynamics-at 0 -> 60 10/2 -> 110 15/2 -> 80)))

        ]
    (concatenate
     phrase-a
     
     (loop-n
      (phrase
       (lily (rstr 5 "<ees f bes>8"))
       (dynamics 70 62 54 71 61)) 6)
     
     (overlay
      phrase-a
      (loop-n phrase-a-alt-bass 2))

     (loop-n
      (phrase
       (lily (rstr 5 "<ees f bes>8"))
       (dynamics 70 62 55 72 62)) 5)

     (overlay
      (phrase
       (lily (rstr 5 "<d g bes>8"))
       (dynamics 72 68 74 82 78))
      (-> (phrase
           (lily "g8 a bes16 c d8 fis" :relative :c3))
          (lift-to-seq ls/overlay-octave-below))
      )
     )

    )
  )



(first (:composition-payload (lily "g8 a bes16 c d8 fis" :relative :c3)))


(def first-bass
  (let [bass-a (concatenate
                (phrase
                 (lily (rstr 4 "ees4. ees4") (rstr 4 "aes4. aes4") :relative :c2)
                 (dynamics-at 0 -> 10 15/2 -> 40 30/2 -> 80))
                (loop-n (phrase
                         (lily  "c8 des d ees4 ees4. ees4" :relative :c2)
                         (dynamics-at 0 -> 80 3/2 -> 120 1.6 80)) 2)
                (loop-n (phrase
                         (lily  "f8 ges g aes4 aes4. aes4" :relative :c2)
                         (dynamics-at 0 -> 80 3/2 -> 120 1.6 80)) 2))
        bass-b   (concatenate
                  (-> (phrase
                       (lily "bes8 aes g16 f bes4
                          ees,8 f g16 ees aes4
                          bes8 aes g16 f bes4
                          ees,8 f g16 ees aes4
      
                          ees'8 des c16 bes aes8 g
                          c8 bes aes16 g d8 f" :relative :c3))
                      (hold-for 0.3)
                      ))
        bass-a-alt (loop-n (concatenate
                            (phrase
                             (lily "c8 des ees f g aes bes c des ees ees,4. ees'4 ees,4. ees'4" :relative :c2)
                             (dynamics-at 0 -> 60 10/2 -> 90)
                             
                             )
                            (phrase
                             (lily "f8 ges aes bes c des ees f ges aes aes,4. aes'4 aes,4. aes'4" :relative :c2)
                             (dynamics-at 0 -> 60 10/2 -> 90)
                             ) ) 2)
        bass-b-alt  (-> phrase
                        (lily "bes8 aes g16 f ees8 d
	                        g f ees16 d c8 bes

                            	bes'8 aes g16 f ees8 d
	                        g f ees16 d c8 bes
	                        ees d c16 bes f8 bes
	                        g8 a bes16 c d8 fis"
                              :relative :c3)
                        (hold-for 0.5)
                        )
        ;;_ (-> bass-b-alt (on-instrument synth-bass) (with-clock clock) (midi-play))
        ]
    (concatenate
     bass-a
     bass-b
     bass-a-alt
     bass-b-alt)
    )
  )


(defn mar-arp [& notes]
  "make that marimba arpeggio from the three note pattern, starting at octave 3"
  (let [note-map (apply hash-map (flatten (map-indexed vector notes)))
        arp  [ [ 0 3 ] [ 1 3 ] [ 2 3 ] [ 1 4 ] [ 0 3 ] [ 2 3 ] [ 0 4 ] [ 1 3 ] [ 2 4 ] [ 0 4 ] ]
        ]
    (phrase 
     (apply pitches (map #(keyword (str (name (get note-map (first %))) (inc (second %)))) arp))
     (apply durations (repeat (count arp) 1/4))
     (dynamics 87 60 62 63 65 64 82 72 65 62)))
  )


(def first-marimba
  (concatenate
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

   )
  )



(def play-it true)
(def player
  (when play-it
    (-> (overlay
         (-> first-lead
             (on-instrument synth-lead))
         (-> first-piano-mid
             (on-instrument piano))
         (-> first-marimba
             (on-instrument marimba))
         (-> first-bass
             (on-instrument synth-bass))
         (-> (mar-arp :ees :c :des)
             (on-instrument marimba))
         )
        ;;(loop-n 4)
        (with-clock clock)
        (midi-play :beat-zero 90))))

;;(def sss (composition-kit.events.physical-sequence/stop player))
