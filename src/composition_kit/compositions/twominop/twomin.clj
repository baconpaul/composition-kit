(ns composition-kit.compositions.twominop.twomin
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

(def instruments
  (-> (midi/midi-instrument-map)
      (midi/add-midi-instrument :vibr (midi/midi-port 0))
      ))


(def clock (tempo/constant-tempo 1 4 98))
(defn try-out [s] (midi-play (-> s (ls/with-clock clock) :beat-clock clock)))


(def vibe
  (let [phrase
        (lily "^hold=0.95 ^i=vibr
g4 <g d' e b'>2 aes8 ees' bes' g' bes, ees,
<< { <a, e' a>2 <aes f' g>} \\\\ { d'4 cis c b} >> g,8 <ees' f bes>
ges, <ees' aes c> <ees aes c> <ees aes c>
aes, <ees' aes c> 
c <ees aes c> <ees aes c> <ees aes c>
des <fes aes fes'> <fes bes fes'> <fes a fes'>
ces <fes des' fes>
bes, <fes des' fes> < fes bes fes'> g
<aes, aes'>2
c8 <ees aes c> <ees aes c> <ees aes c>
des <fes aes fes'> <fes bes fes'> <fes a fes'>
ces <fes des' fes>
bes, <fes des' fes> < fes bes fes'> g
<aes, aes'>2
" :relative :c4 :instruments instruments)]
    phrase
    )
  )
;;

(def final
  (->
   (<*>
    vibe
    )
   (ls/with-clock clock)
   )
  )

(def ag
  (midi-play
   final
   :beat-clock clock
   :beat-zero -1))


