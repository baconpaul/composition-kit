(ns composition-kit.physical-to-logical
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.logical-sequence :as ls])
  )

(defn schedule-logical-on-physical
  [in-seq pattern instrument clock]
  ;; This is basically a massive reduce statement on a big switch based on item type which then
  ;; does the magic
  (reduce (fn [pseq item]
            (case (:itemtype item)
              :composition-kit.logical-sequence/notes-with-duration
              (let [payload  (ls/item-payload item)
                    notecont (:notes payload)
                    notes    (if (coll? notecont) notecont [ notecont ] )
                    hold-for (:hold-for payload)
                    start-time (* 1000 (tempo/beats-to-time clock (ls/item-beat item)))
                    end-time   (* 1000 (tempo/beats-to-time clock (+ hold-for (ls/item-beat item))))
                    ons      (reduce (fn [s e] (ps/add-to-sequence s (fn [t] (println "note on " e " " t )) start-time)) pseq notes)
                    offs     (reduce (fn [s e] (ps/add-to-sequence s (fn [t] (println "note off " e " " t)) end-time)) ons notes)
                    ]
                offs
                )
              )
            )
          in-seq
          pattern)
  )
