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



;; This actually sequences properly!
(def aa
  (let [rcv (midi-util/getOpenedReceiver)
        sd  (partial midi-util/gen-send rcv)
        ]
    (-> (ms/new-sequence)
        (ms/->sequence 
         (sd (midi-util/note-on 1 60 100)) 500
         (sd (midi-util/note-on 1 61 100)) 520
         (sd (midi-util/note-off 1 60)) 1000
         (sd (midi-util/note-off 1 61)) 2000
         ))))

(ms/play aa)



;; (println (agent-error aa))

;;(agent-error aa)

(ms/stop aa)




