(ns composition-kit.physical-to-logical
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.logical-sequence :as ls])
  (:require [composition-kit.tonal-theory :as th])
  )

(defn ^:private schedulable-item [item]
  (let [instrument (ls/item-instrument item)
        clock      (ls/item-clock item)
        _          (when (nil? clock) 
                     (throw (ex-info "Item with nil clock can't be scheduled"
                                     {:item item})))
        _          (when (nil? instrument) 
                     (throw (ex-info "Item with nil instrument can't be scheduled"
                                     {:item item})))]
    true))

(defn schedule-logical-on-physical
  [in-seq pattern]
  ;; This is basically a massive reduce statement on a big switch based on item type which then
  ;; does the magic
  (reduce (fn [pseq item]
            (case (ls/item-type item)
              :composition-kit.logical-sequence/notes-with-duration
              (let [_          (schedulable-item item)
                    payload    (ls/item-payload item)
                    instrument (ls/item-instrument item)
                    clock      (ls/item-clock item)
                    
                    notecont (:notes payload)
                    notes    (if (coll? notecont) notecont [ notecont ] )
                    resolved-notes (map th/note-by-name notes)
                    hold-for (:hold-for payload)
                    lev      (ls/note-dynamics-to-7-bit-volume item)
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
                                  lev)
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
              
              :composition-kit.logical-sequence/control-event
              (let [_          (schedulable-item item)
                    payload    (ls/item-payload item)
                    clock      (ls/item-clock item)
                    instrument (ls/item-instrument item)

                    start-time (* 1000 (tempo/beats-to-time clock (ls/item-beat item)))]
                (ps/add-to-sequence
                 pseq
                 (midi/send-control-change
                  (:receiver instrument)
                  (:channel instrument)
                  (:control payload)
                  (:value payload))
                 start-time
                 ))
              
              :composition-kit.logical-sequence/rest-with-duration
              pseq
              )
            )
          in-seq
          pattern)
  )


(defn create-and-schedule [pattern]
  "A utility for when you want just one sequence schedulable"
  (-> (ps/new-sequence)
      (schedule-logical-on-physical pattern)))


