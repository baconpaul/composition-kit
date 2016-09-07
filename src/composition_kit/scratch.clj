(ns composition-kit.scratch
  (:require [composition-kit.midi-util :as midi-util])
  (:require [composition-kit.music-sequence :as ms])
  )

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


(-> (ms/new-sequence)
    (ms/->sequence 
     (fn[rt] (println "hi 2 " rt)) 2000
     (fn[rt] (println "hi 3 " rt)) 3000
     (fn[rt] (println "hi 1 " rt)) 1000))




(def aa (-> (ms/new-sequence)
            (ms/->sequence 
             (fn[rt] (println "hi 2 " rt)) 3000
             (fn[rt] (println "hi 3 " rt)) 5000
             (fn[rt] (println "hi 1 " rt)) 1000)
            ms/play
            ;; ( (fn [a] (await a) @a) )
            ))

;; (println (agent-error aa))

;;(agent-error aa)

(ms/stop aa)




