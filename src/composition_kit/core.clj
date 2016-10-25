(ns composition-kit.core
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])
  (:require [composition-kit.events.physical-sequence :as ps])
  (:require [composition-kit.music-lib.parse :as parse])
  (:require [composition-kit.music-lib.logical-to-physical :as ltop])
  )



(def <*> ls/merge-sequences)
(def >>> ls/concat-sequences)
(defn rest-for [dur]
  (ls/concrete-logical-sequence [(i/rest-with-duration dur 0)]))

(defn lily [ & arguments ]
  "The lily macro allows you to make a component which participates in a phrase based on a simple monophonic lilypond type list
of symbols. For instance

   (phrase
      (lily :relative :c4 e2 d4 c f16 e d c))"
  (let [lily-bits     (filter string? arguments)
        lily-string   (apply str (interpose " " lily-bits))
        control-bits  (filter (comp keyword?) arguments)
        ]
    (apply parse/lily-to-logical-sequence (concat [ lily-string ] control-bits))
    )
  )

(defn step-strings [ & arguments ]
  (let [args           (if (keyword? (first arguments)) arguments (first arguments))
        note-str-pairs (partition 2 args)
        _ (if-not
              (and
               (every? #(and (keyword? (first %)) (string? (second %))) note-str-pairs)
               (= (mod (count args) 2) 0))
            (throw (ex-info "Arguments need to be note/pattern pairs" { :args args } )))
        ]
    (apply ls/merge-sequences 
           (map (fn [[note str]]
                  (parse/str->n note str)) note-str-pairs))
    )
  )


(defn midi-play [ item & opt-arr ]
  (let [target item
        ps      (-> (ps/new-sequence)
                    (as-> s
                        ;;(ltop/schedule-logical-on-physical (:composition-payload item)))]
                        (apply ltop/schedule-logical-on-physical (concat [ s item ] opt-arr))))]
    (ps/play ps :user-stop midi/all-notes-off)
    
    )
  )

