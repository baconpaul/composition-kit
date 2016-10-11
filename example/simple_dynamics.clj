(ns simple-dynamics
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.logical-sequence :as ls])
  (:require [composition-kit.tonal-theory :as th])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.parse :as parse])
  (:require [composition-kit.physical-to-logical :as ptol])
  )

;; The first few measures of the adiago cantabile from beethoven op 13

(def piano (midi/midi-instrument 0))
(def clock (tempo/constant-tempo 4 4 120))

(def some-music (ls/loop-sequence
                 (ls/sequence-from-pitches-constant-duration
                  (th/scale-pitches :c4 :major)
                  1/2)
                 2))


(def line-crescendo (ls/line-segment-dynamics some-music 0 20 8 127))
(def up-down (ls/line-segment-dynamics some-music 0 30 4 127 8 70))

(defn on-p-i [s] (-> s (ls/assign-instrument piano) (ls/assign-clock clock)))

(def which-to-play :c)
(case which-to-play
  :a (ps/play (ptol/create-and-schedule (on-p-i some-music)))
  :b (ps/play (ptol/create-and-schedule (on-p-i line-crescendo)))
  :c (ps/play (ptol/create-and-schedule (on-p-i up-down)))

  :d (ps/play (ptol/create-and-schedule
               (on-p-i (parse/lily-to-logical-sequence "c8 d e4 e fis16 f e ees c8 r c ees d4 c4 c' c,2")))))

