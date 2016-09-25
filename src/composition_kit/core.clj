(ns composition-kit.core
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.logical-sequence :as ls])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.parse :as parse])
  (:require [composition-kit.physical-to-logical :as ptol])
  )


(defn phrase [ & arguments ]
  (let [by-type  (group-by :composition-type arguments)
        types    (set (keys by-type))]
    {:composition-type ::sequence
     :composition-payload 
     (-> (ls/concrete-logical-sequence [])
         ;; So do we have any sequences? If so they win
         ((fn [s] (if (::sequence types) (apply ls/merge-sequences (map :composition-payload (::sequence by-type))) s)))
         
         ;; Do we have a pitch and duration pair? If so merge that on
         ((fn [s] (if (and (::pitches types) (::durations types))
                    (if (and (= 1
                                (count (::pitches by-type))
                                (count (::durations by-type))))
                      (do
                        (println (::pitches by-type))
                        (println (::durations by-type))
                        (ls/merge-sequences s (ls/sequence-from-pitches-and-durations
                                               (:composition-payload (first (::pitches by-type)))
                                               (:composition-payload (first (::durations by-type)))))
                        )
                      (throw (ex-info "One and only one pitch and duration statement allowed in a phrase, sorry", { ::pitches (::pitches by-type)
                                                                                                                   ::durations (::durations by-type)})))
                    s))))}
    
    )
  )




(defn lily [ & arguments ]
  "The lily macro allows you to make a component which participates in a phrase based on a simple monophonic lilypond type list
of symbols. For instance

   (phrase
      (lily :relative :c4 e2 d4 c f16 e d c))"
  (let [lily-bits     (filter string? arguments)
        lily-string   (apply str (interpose " " lily-bits))
        control-bits  (filter (comp keyword?) arguments)
        ]
    {:composition-type ::sequence :composition-payload (apply parse/lily-to-logical-sequence (concat [ lily-string ] control-bits))}
    )
  )

(defn pitches [ & arguments ]
  {:composition-type ::pitches :composition-payload arguments }
  )

(defn durations [ & arguments ]
  {:composition-type ::durations :composition-payload arguments }
  )

(defn dynamics [ & arguments ]
  {:composition-type ::dynamics :dynamics-type ::explicit :composition-payload arguments }
  )

(defmacro dynamics-at [ & arguments ]
  "The dynamics-at macro allows you to augment a phrase with information about dynamics at certain beat points. It has the syntax of beat -> volume pairs.
For instance:

  (phrase
    (lily :relative :c4 e2 d4 c e1)
    (dynamics-at  0 -> 120 1 -> 60 3 -> 80))"
  {:composition-type ::dynamics :dynamics-type ::function :composition-payload arguments }
  )

(defn concatenate [ & arguments ]
  (if (not (every? #(= (:composition-type %) ::sequence) arguments))
    (throw (ex-info "Can only conctenate sequences. You handed me these types"
                    { :types (map :composition-type arguments) }))
    {:composition-type ::sequence :composition-payload (apply ls/concat-sequences (map :composition-payload arguments))}
    )
  )

(defn overlay [ & arguments ]
  (if (not (every? #(= (:composition-type %) ::sequence) arguments))
    (throw (ex-info "Can only overlay sequences. You handed me these types"
                    { :types (map :composition-type arguments) :args arguments }))
    {:composition-type ::sequence :composition-payload (apply ls/merge-sequences (map :composition-payload arguments))}
    )
  )

(defmacro pedal-held-and-cleared-at [ & arguments ]
  {:composition-type ::sequence :composition-payload [] }
  )

(defn midi-play [ sequence on clock ]
  (if (not (= (:composition-type sequence) ::sequence)) (throw (ex-info "I can only midi-play a sequence" { :sequence-was sequence }))
      (let [target  (:composition-payload sequence)
            ps      (-> (ps/new-sequence)
                        (ptol/schedule-logical-on-physical target on clock))]
        (ps/play ps)
        
        )
      )
  )

(:d (set (keys (group-by :t (list { :t :a :v 1 } { :t :b :v 1 } {:t :a :v 2 })))))
