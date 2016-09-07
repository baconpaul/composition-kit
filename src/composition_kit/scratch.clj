(ns composition-kit.scratch
  (:require [composition-kit.midi-util :as midi-util]))

(defn playNote
  [rcv chan pitch velocity dur]
  (let [nOn  (midi-util/note-on chan pitch velocity)
        nOff (midi-util/note-off chan pitch)]
    (.send rcv nOn -1)
    (Thread/sleep dur)
    (.send rcv nOff -1)
    pitch
    )
  )


(map #(playNote (midi-util/getOpenedReceiver) 1 % 80 1000) [60 63 67])


;;; Sorted Set Noodling
;;(def xx (sorted-set))
(-> (sorted-set)
    (conj 8)
    (conj 3)
    (conj 2)
    first
    )
