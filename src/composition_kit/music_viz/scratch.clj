(ns composition-kit.music-viz.scratch
  (:import (org.baconpaul.pianoroll BoxSeries PianoRoll))
  (:import (org.jfree.data.xy XYIntervalSeriesCollection))
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as li])
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:use composition-kit.core)
  )


(defn ls-to-boxseries-collection [s]
  (let [by-inst (group-by #(:name (li/item-instrument %)) s )
        
        item-onto-series (fn [item box-series]
                           (when (= (li/item-type item) :composition-kit.music-lib.logical-item/notes-with-duration)
                             (let [p (li/item-payload item) ;; get all the notes out of this. Do that!
                                   _ (println p)
                                   notes (if (seq? (:notes p)) (:notes p) [ (:notes p) ])
                                   _ (println notes)
                                   b (li/item-beat item)
                                   d (:hold-for p)
                                   v (li/note-dynamics-to-7-bit-volume item)]
                               (doseq [note notes]
                                 (let [n (-> note
                                             (th/note-by-name)
                                             :midinote
                                             )]
                                   (println n b d v)
                                   (doto box-series
                                     (.add b b (+ b d) n (- n 0.5) (+ n 0.5)  (/ v 127))
                                     )))
                               )
                             )
                           )

        box-series-list
        (for [ [iname ils] by-inst ]
          (let [bs (BoxSeries. (name iname))]
            (doseq [item ils] (item-onto-series item bs))
            bs
            )
          )

        series-collection
        (let [res (XYIntervalSeriesCollection.)]
          (doseq [s box-series-list]
            (.addSeries res s)
            )
          res
          )
        


        ]
    series-collection
    )
  )


(defn show-sequence [s]
  (let [co (ls-to-boxseries-collection s)
        pr (PianoRoll. "Demo" co)
        
        ]
    (doto pr
      (.pack)
      (.setVisible true
                   ))
    )
  )

