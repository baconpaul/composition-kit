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
  (concatenate
   (-> 
    (str
     (rstr 2 "bes'4. bes4 bes16 aes g8 f c ees")
     (rstr 2 "ees'4. ees4 ees16 des c8 bes ges aes")
     "bes4. bes4 bes16 aes g8 f c ees "
     "bes'4. bes4 bes16 aes g8 f c ees "
     (rstr 2 "ees'4. ees4 ees16 des c8 bes ges aes"))
    lily
    ;; make those notes stacatto when...
    (lift-to-seq
     ls/apply-note-payload-transform
     (fn [i p] (if (>= (:dur p) 1) p
                   (assoc p :hold-for 0.1))))
    )
   (-> (phrase
        (lily " bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      
      ees'8 des c16 bes aes8 g
      c8 bes aes16 g d8 f" :relative :c5)
        (dynamics-at 0 -> 90 2.4 -> 80
                     2.5 -> 100 4.9 -> 80)
        )
       (lift-to-seq
        ls/apply-note-payload-transform
        (fn [i p] (assoc p :hold-for 0.15)))
       )

   )
  )



(def first-piano-mid
  (concatenate
   (phrase 
    (lily (rstr 4 "<ees c' des>4. <ees c' des>4" ) :relative :c3)
    (dynamics-at 0 -> 70 3/2 -> 60 5/2 -> 80 5 -> 70 15/2 -> 65)
    (pedal-held-and-cleared-at 0 5 10)
    )
   (phrase
    (lily (rstr 4 "<aes f' ges>4. <aes f' ges>4" ) :relative :c4)
    (dynamics-at 0 -> 70 3/2 -> 60 5/2 -> 80 5 -> 70 15/2 -> 65)
    (pedal-held-and-cleared-at 0 5 9.5)
    )
   (phrase
    (lily (rstr 4 "<ees c' des>4. <ees c' des>4" ) :relative :c3)
    (dynamics-at 0 -> 70 3/2 -> 60 5/2 -> 80 5 -> 70 15/2 -> 65)
    (pedal-held-and-cleared-at 0 5 9.5)
    )
   (phrase
    (lily (rstr 4 "<aes f' ges>4. <aes f' ges>4" ) :relative :c4)
    (dynamics-at 0 -> 70 3/2 -> 60 5/2 -> 80 5 -> 70 15/2 -> 65)
    (pedal-held-and-cleared-at 0 5 9.5)
    )
   (loop-n
    (phrase
     (lily (rstr 5 "<ees f bes>8"))
     (dynamics 80 66 64 81 65)) 6)
   ))

(def first-bass
  (concatenate
   (phrase
    (lily (rstr 4 "ees4. ees4") (rstr 4 "aes4. aes4") :relative :c2)
    (dynamics-at 0 -> 10 15/2 -> 40 30/2 -> 80))
   (loop-n (phrase
            (lily  "c8 des d ees4 ees4. ees4" :relative :c2)
            (dynamics-at 0 -> 80 3/2 -> 120 1.6 80)) 2)
   (loop-n (phrase
            (lily  "f8 ges g aes4 aes4. aes4" :relative :c2)
            (dynamics-at 0 -> 80 3/2 -> 120 1.6 80)) 2)
   (phrase
    (lily " bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      bes8 aes g16 f bes4
      ees,8 f g16 ees aes4
      
      ees'8 des c16 bes aes8 g
      c8 bes aes16 g d8 f" :relative :c3))
   ))

(defn mar-arp [& notes]
  "make that marimba arpeggio from the three note pattern, starting at octave 3"
  (let [note-map (apply hash-map (flatten (map-indexed vector notes)))
        arp  [ [ 0 3 ] [ 1 3 ] [ 2 3 ] [ 1 4 ] [ 0 3 ] [ 2 3 ] [ 0 4 ] [ 1 3 ] [ 2 4 ] [ 0 4 ] ]
        ]
    (phrase 
     (apply pitches (map #(keyword (str (name (get note-map (first %))) (inc (second %)))) arp))
     (apply durations (repeat (count arp) 1/4))
     (dynamics 87 60 62 63 65 64 81 72 65 62)
     )
    
    ))

(def first-marimba
  (concatenate
   (loop-n (mar-arp :ees :c :des) 4)
   (loop-n (mar-arp :aes :f :ges) 4)
   (loop-n (mar-arp :ees :c :des) 4)
   (loop-n (mar-arp :aes :f :ges) 4)
   (loop-n (mar-arp :ees :f :bes) 6)
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
        (with-clock clock)
        (midi-play))))

;;(composition-kit.events.physical-sequence/stop player)
