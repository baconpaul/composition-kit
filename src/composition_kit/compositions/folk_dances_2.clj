(ns composition-kit.compositions.folk-dances-2
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])

  (:use composition-kit.core))


(def piano (midi/midi-instrument 1))

(def clock (tempo/constant-tempo 5 4 95))

(def piano-rh-as-written
  "ees2 d2.
  ees4 f g2.
  aes2 g2.
  
  f4 g ees d
  c d bes1
  
  ees2 f2.
  g4 aes bes2.
  aes2 g2.
  
  f4 g ees d
  c d bes1
  
  bes2 ees2.
  d2 ees2.
  bes2 ees2.
  d2 aes'2 g4
  f g ees2.
  d4 ees c d bes
  
  bes2 ees2.
  d2 ees2.
  bes2 ees2.
  d2 aes'2 g4
  f g ees2.
  d4 ees c d bes
  
  <c ees aes>2 <c ees g>4 <c ees f> <c ees g> 
  <c ees> <c f> <c d> <c ees> bes
  bes <f bes ees>1
  bes4 <f bes ees>1
  
  bes4 << { aes'4 g f ees } \\\\ < g, bes >1 >>
  bes4 << { d4 ees c bes } \\\\ < g bes >1 >>
  bes4 << { aes'4 g f ees } \\\\ < g, bes >1 >>
  bes4 << { d4 ees c bes } \\\\ < g bes >1 >>

  bes4 <f bes ees>1
  bes4 <f bes ees>1
  
  bes4 << <f bes>1 \\\\ { ees'2 d }>>
  <g, bes ees>1"
  )

(def piano-lh-as-written "
  <bes a g>2 <bes a ges>2.
  <bes a g>4 <bes a e>4 <bes a d,>2.
  <bes a g>2 <bes a ges>2.

  <bes a g>4 <bes a ges>4 <bes a d,>4 <bes a e>
  <bes a ges>4 <bes a g>4 <bes a e>1

  <bes a g>2 <bes a ges>2.
  <bes a g>4 <bes a e>4 <bes a d,>2.
  <bes a g>2 <bes a ges>2.

  <bes a g>4 <bes a ges>4 <bes a d,>4 <bes a e>
  <bes a ges>4 <bes a g>4 <bes a e>1

  <bes a g>2 <bes a ges>2.
  <bes a g>2 <bes a e>2.
  <bes a g>2 <bes a ges>2.
  <bes a g>2 <bes a e>2.
  <bes a g>2 <bes a ges>2.
  <bes a g>4 <bes a ges> <bes a f> <bes a e> <bes a ees>

  <bes a g>2 <bes a ges>2.
  <bes a g>2 <bes a e>2.
  <bes a g>2 <bes a ges>2.
  <bes a g>2 <bes a e>2.
  <bes a g>2 <bes a ges>2.
  <bes a g>4 <bes a ges> <bes a f> <bes a e> <bes a ees>

  bes2 a4 aes g ges f ees des c
  bes <ges ges'>1
  bes4 <ges ges'>1
  bes4 <ges ges'>1
  bes4 <ges ges'>1 
  bes4 <ges ges'>1
  bes4 <ges ges'>1


  bes4 <ges ges'>1
  bes4 <des, des'>1
  bes'4 <bes, bes'>1
  <ees, ees'>1")

(def player
  (->
   (-> (overlay
        (lily piano-rh-as-written :relative :c4)
        (lily piano-lh-as-written :relative :c3))
       (on-instrument piano))
   (with-clock clock)
   (midi-play)))

(def x (composition-kit.events.physical-sequence/stop player))

