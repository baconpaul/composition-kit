(ns composition-kit.music-lib.logical-to-physical
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.events.physical-sequence :as ps])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  )

(defn ^:private schedulable-item [item]
  (let [instrument (i/item-instrument item)
        clock      (i/item-clock item)
        _          (when (nil? clock) 
                     (throw (ex-info "Item with nil clock can't be scheduled"
                                     {:item item})))
        _          (when (nil? instrument) 
                     (throw (ex-info "Item with nil instrument can't be scheduled"
                                     {:item item})))]
    true))

(defn schedule-logical-on-physical
  [in-seq in-pattern & opt-arr]
  ;; This is basically a massive reduce statement on a big switch based on item type which then
  ;; does the magic
  (let [opts (apply hash-map opt-arr)
        beat-zero (get opts :beat-zero 0)
        pattern  (drop-while #(< (i/item-beat %) beat-zero) in-pattern)
        ]
    (reduce (fn [pseq item]
              (case (i/item-type item)
                :composition-kit.music-lib.logical-item/notes-with-duration
                (let [_          (schedulable-item item)
                      payload    (i/item-payload item)
                      instrument (i/item-instrument item)
                      clock      (i/item-clock item)
                      t0         (tempo/beats-to-time clock beat-zero)
                      
                      notecont (:notes payload)
                      notes    (if (coll? notecont) notecont [ notecont ] )
                      resolved-notes (map th/note-by-name notes)
                      hold-for (:hold-for payload)
                      lev      (i/note-dynamics-to-7-bit-volume item)
                      start-time (* 1000 (- (tempo/beats-to-time clock (i/item-beat item)) t0))
                      end-time   (* 1000 (- (tempo/beats-to-time clock (+ hold-for (i/item-beat item))) t0))
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
                
                :composition-kit.music-lib.logical-item/control-event
                (let [_          (schedulable-item item)
                      payload    (i/item-payload item)
                      clock      (i/item-clock item)
                      t0         (tempo/beats-to-time clock beat-zero)
                      instrument (i/item-instrument item)
                      
                      start-time (* 1000 (- (tempo/beats-to-time clock (i/item-beat item)) t0))]
                  (ps/add-to-sequence
                   pseq
                   (midi/send-control-change
                    (:receiver instrument)
                    (:channel instrument)
                    (:control payload)
                    (:value payload))
                   start-time
                   ))
                
                :composition-kit.music-lib.logical-item/rest-with-duration
                pseq
                )
              )
            in-seq
            pattern)
    )
  )



(defn create-and-schedule [pattern]
  "A utility for when you want just one sequence schedulable"
  (-> (ps/new-sequence)
      (schedule-logical-on-physical pattern)))


