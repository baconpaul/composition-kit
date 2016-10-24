(ns composition-kit.compositions.folk-dances.folk-dances-3
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

(def piano (midi/midi-instrument 0))
(def clock (tempo/constant-tempo 15 4 95))

(def orig-piano-rh
  "<c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2

  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2

  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2
  
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2
  
  <g' bes ees>4 <g bes g'> <g bes ees> <c ees c'>1
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>2. <c ees g>2
  
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>1
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>2. <c ees g>2

  <g des ees>4 <g des g> <g des ees> <ces ees ces'>1
  <g des ees>4 <g des g> <g des ees> <ces ees ces'>2. <c ees c'>2
  
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>1
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>2. <c ees g>2

  <des ees bes'>4 <des ees aes> <des ees> <g, bes ees>1
  <c ees aes>4 <c ees g> c <f, aes des>2. <g aes ees'>2
  
  <aes bes ees>4 <aes bes ees> <aes bes ees> <g aes ees'>1
  <g aes ees'>4 <g aes ees'> <g aes ees'> <aes bes ees>2. <ees' bes'>2
  
  <ees c'>4 <ees c'> <ees c'> <ees aes bes>1 <ees c'>4 <ees c'> <ees c'> <aes des>2. <g, aes ees'>2

  <c, ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2

  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2

  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2
  
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees aes>1")

(def orig-piano-lh
  "
   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   des4 des des des1 des4 des des des2. des2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2

   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   des4 des des des1 des4 des des des2. des2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2

  des4 des des ees f g bes aes aes aes f, aes des f aes
  
   g4 ees bes c1 des4 ees f g aes bes c2
   des4 des des c1 des4 des des ees2. ees,2
  
   aes,4 aes aes aes1 aes4 aes aes aes2. aes2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   des4 des des des1 des4 des des des2. des2
   aes4 aes aes aes1 aes4 aes aes aes1" )



(def orig-piano
  (let [in-measure-dyn      (partition 2 [0 1  4 1.2  5 0.8  11 1.15  12 0.6]) ;; this pattern is bad for measures 9 - 11
        mid-measure-dyn     (partition 2 [0 1 3 0.9 6 1.05 6.9 1.02 9 0.97 15 1.15])
        final-mid-measure   (partition 2 [0 0.95 10 1.2 12 0.8])

        by-measure-dyn      (concat (repeat 8 in-measure-dyn)
                                    (repeat 2 mid-measure-dyn)
                                    (repeat 1 final-mid-measure)
                                    (repeat 4 in-measure-dyn))
        
        measure-overall-dyn [45 47 52 47 63 64 67 64 72 76 79 45 47 52 46]
        xpand-dyn (flatten
                   (map (fn [id md inm]
                          (let [ofs (* id 15)]
                            (map (fn [i] [ (+ ofs (first i)) (* (second i) md)]) inm)))
                        (range (count measure-overall-dyn)) measure-overall-dyn by-measure-dyn))

        rh-orig
        (-*> (lily orig-piano-rh :relative :c3)
             (ls/hold-for-pct 0.99)
             ((fn [s] (apply ls/line-segment-dynamics (concat [s] xpand-dyn)))))


        nff
        (fn [op]
          (fn [i p]
            (if (seq? (:notes p))
              ;; find the highest
              (let [n (:notes p)
                    mn (map th/name-to-midinote n)
                    top (apply max mn)
                    r   (op #(= (th/name-to-midinote %) top) n)
                    ]
                (assoc p :notes r))
              p)))
        
        rh-top-note
        (-*> rh-orig
             (ls/transform-note-payload (nff filter)))

        rh-other-notes 
        (-*> rh-orig
             (ls/transform-note-payload (nff remove)))

        rh
        (<*>
         (-*> rh-top-note (ls/amplify 1.05))
         (-*> rh-other-notes (ls/amplify 0.95)))
        
        lh
        (let [ls (lily orig-piano-lh :relative :c3)]
          (-*>
           (<*> ls
                (-*> ls (ls/transpose -12)))
           (ls/hold-for-pct 0.98)
           ((fn [s] (apply ls/line-segment-dynamics (concat [s] xpand-dyn))))
           (ls/amplify 0.87)
           )
          )

        ]
    
    (-*>
     (<*>
      rh
      lh
      (apply pedal-held-and-cleared-at (mapcat (fn [i] [(* 15 i) (+ (* 15 i) 3) (+ (* 15 i) 7) (+ (* 15 i) 10)]) (range 15)))
      )

     )
    ))


(def final-song
  (->
   (<*>
    (-> orig-piano (on-instrument piano))
    )
   (with-clock clock)))

(def playit true)
(def player
  (when playit
    (midi-play
     final-song
     ;;:beat-zero (* 8 15)
     ;;t:beat-end (* 12 15)
     )))

;;(def s (composition-kit.events.physical-sequence/stop player))


;;(map (juxt i/item-beat i/item-payload) (:composition-payload (pedal-held-and-cleared-at 1 3 5)))


