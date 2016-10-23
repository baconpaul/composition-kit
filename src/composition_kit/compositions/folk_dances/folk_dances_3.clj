(ns composition-kit.compositions.folk-dances.folk-dances-3
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
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
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees aes>1 r4")

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
   aes4 aes aes aes1 aes4 aes aes aes1 r4" )


(def orig-piano
  (-*>
   (<*>
    (-*> (lily orig-piano-rh :relative :c3)
         (ls/hold-for-pct 0.98)
         )
    (let [ls (lily orig-piano-lh :relative :c3)]
      (-*>
       (<*> ls
            (-*> ls (ls/transpose -12)))
       (ls/hold-for-pct 0.98)
       ))
    (apply pedal-held-and-cleared-at (mapcat (fn [i] [(* 15 i) (+ (* 15 i) 3) (+ (* 15 i) 7) (+ (* 15 i) 10)]) (range 15)))
    )
   (ls/transform :dynamics (fn [i] (constantly 64)))
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
     :beat-zero 70
     ;;:beat-end 120
     )))

;;(def s (composition-kit.events.physical-sequence/stop player))





