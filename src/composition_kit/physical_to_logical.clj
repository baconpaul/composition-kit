(ns composition-kit.physical-to-logical
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.logical-sequence :as ls])
  (:require [composition-kit.tonal-theory :as th])
  )

(defn schedule-logical-on-physical
  [in-seq pattern instrument clock]
  ;; This is basically a massive reduce statement on a big switch based on item type which then
  ;; does the magic
  (reduce (fn [pseq item]
            (case (ls/item-type item)
              :composition-kit.logical-sequence/notes-with-duration
              (let [payload  (ls/item-payload item)
                    notecont (:notes payload)
                    notes    (if (coll? notecont) notecont [ notecont ] )
                    resolved-notes (map th/note-by-name notes)
                    hold-for (:hold-for payload)
                    start-time (* 1000 (tempo/beats-to-time clock (ls/item-beat item)))
                    end-time   (* 1000 (tempo/beats-to-time clock (+ hold-for (ls/item-beat item))))
                    ons      (reduce
                              (fn [s e]
                                (ps/add-to-sequence
                                 s
                                 ;;(partial println "NOTE ON" e)
                                 (midi/send-note-on
                                  (:receiver instrument)
                                  (:channel instrument)
                                  (:midinote e)
                                  100)
                                 start-time))
                              pseq
                              resolved-notes
                              )

                    offs     (reduce
                              (fn [s e]
                                (ps/add-to-sequence
                                 s
                                 ;;(partial println "NOTE Off" e)
                                 (midi/send-note-off
                                  (:receiver instrument)
                                  (:channel instrument)
                                  (:midinote e))
                                 end-time)
                                )
                              ons
                              resolved-notes)
                    ]
                offs
                )
              )
            )
          in-seq
          pattern)
  )


(defn create-and-schedule [pattern instrument clock]
  "A utility for when you want just one sequence schedulable"
  (-> (ps/new-sequence)
      (schedule-logical-on-physical pattern instrument clock)))

