(ns composition-kit.compositions.folk-dances.folk-dances-3
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

(def piano (midi/midi-instrument 1))
(def sin-bell (midi/midi-instrument 1))
(def clock (tempo/constant-tempo 15 4 95))

(defn try-out [p i]
  (-> p (ls/on-instrument i) (ls/with-clock clock) (midi-play)))



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
  (let [in-measure-dyn      (partition 2 [0 1  4 1.2  5 0.8  11 1.15  12 0.5]) ;; this pattern is bad for measures 9 - 11
        mid-measure-dyn     (partition 2 [0 1 3 0.9 6 1.05 6.9 1.02 9 0.97 15 1.15])
        final-mid-measure   (partition 2 [0 0.95 10 1.2 12 0.8])

        by-measure-dyn      (concat (repeat 8 in-measure-dyn)
                                    (repeat 2 mid-measure-dyn)
                                    (repeat 1 final-mid-measure)
                                    (repeat 4 in-measure-dyn))
        
        measure-overall-dyn [42 45 57 47 63 64 67 64 72 76 79 45 47 52 46]
        xpand-dyn (flatten
                   (map (fn [id md inm]
                          (let [ofs (* id 15)]
                            (map (fn [i] [ (+ ofs (first i)) (* (second i) md)]) inm)))
                        (range (count measure-overall-dyn)) measure-overall-dyn by-measure-dyn))

        rh-orig
        (-> (lily orig-piano-rh :relative :c3)
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
        (-> rh-orig
            (ls/transform-note-payload (nff filter)))

        rh-other-notes 
        (-> rh-orig
            (ls/transform-note-payload (nff remove)))

        rh
        (<*>
         (-> rh-top-note (ls/amplify 1.07))
         (-> rh-other-notes (ls/amplify 0.92)))
        
        lh
        (let [ls (lily orig-piano-lh :relative :c3)]
          (->
           (<*> ls
                (-> ls (ls/transpose -12)))
           (ls/hold-for-pct 0.98)
           ((fn [s] (apply ls/line-segment-dynamics (concat [s] xpand-dyn))))
           (ls/amplify 0.82)
           )
          )

        ]
    
    (->
     (<*>
      rh
      lh
      (apply ls/pedal-held-and-cleared-at (mapcat (fn [i] [(* 15 i) (+ (* 15 i) 3) (+ (* 15 i) 7) (+ (* 15 i) 10)]) (range 15)))
      )

     )
    ))


;; overlayone is (r8 root 5 root o 5) 6 pattern shifting across
(def sin-bell-repeat
  (let [ph  (->
             (lily "r8 aes ees' aes, aes'16 aes' ees,8" :relative :c5)
             (ls/hold-for-pct 0.2)
             )
        ph2  (->
              (lily "r8 aes16 aes' ees, ees' aes, aes' aes aes' ees, ees'" :relative :c5)
              (ls/hold-for-pct 0.2)
              )
        ;;_ (try-out ph sin-bell)
        ]
    (>>>
     (rest-for (* 2 15))
     (->
      (ls/loop-n ph 10)
      (ls/line-segment-dynamics 0 0 (* 2 15) 30))
     (->
      (ls/loop-n ph2 20)
      (ls/line-segment-dynamics 0 30 (* 4 15) 20)
      )
     )
    ))

(def final-song
  (->
   (<*>
    (-> orig-piano (ls/on-instrument piano))
    ;;(-> sin-bell-repeat (on-instrument sin-bell))
    )
   (ls/with-clock clock)))

(def playit true)
(def player
  (when playit
    (midi-play
     final-song
     :beat-zero (- (* 0 15) 1)
     :beat-end (* 2 15)
     )))

;;(def s (composition-kit.events.physical-sequence/stop player))




