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
         ((fn [s] (if (or (::pitches types) (::durations types))
                    (if (and (= 1
                                (count (::pitches by-type))
                                (count (::durations by-type))))
                      (ls/merge-sequences s (ls/sequence-from-pitches-and-durations
                                             (:composition-payload (first (::pitches by-type)))
                                             (:composition-payload (first (::durations by-type)))))
                      (throw (ex-info "One and only one pitch and duration statement allowed in a phrase, sorry", { ::pitches (::pitches by-type)
                                                                                                                   ::durations (::durations by-type)})))
                    s)))

         ;; Do we have any dynamics
         ((fn [s] (if  (::dynamics types)
                    (do
                      (when (> (count (::dynamics by-type)) 1)
                        (throw (ex-info "One and only one dynamics in a phrase, sorry", { ::dynamics (::dynamics by-type)})))
                      (let [ dyn  (first (::dynamics by-type)) ]
                        (condp = (:dynamics-type dyn)
                          ::explicit
                          (ls/explicit-segment-dynamics s (:composition-payload dyn))
                          ::function
                          ((:composition-payload dyn) s)
                          )
                        ))
                    ;; else
                    s)))
         )
     }
    
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
  (let [arg-group (partition 3 arguments)]
    (when-not (every? (fn [ [ b s l ] ] (and (= (type b) (type l) java.lang.Long)  (= s '->))) (partition 3 arguments))
      (throw (ex-info "Incorrect syntax. Should be b -> l b -> l. You gave me arguments as shown." { :args arguments } )))
    (let [line-seg (mapcat (fn [ [ b s l ] ] [ b l ] ) arg-group)
          argname (gensym 'seq_)]
      `{:composition-type ::dynamics :dynamics-type ::function :composition-payload
        (fn [ ~argname ] (ls/line-segment-dynamics ~argname ~@line-seg))}
      )
    )
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

(defn pedal-held-and-cleared-at [ & arguments ]
  "Given a collection of beats, depress the pedal just a smidge after the beat and then
hold it until the next beat, where it releases and re-applies. So basically pedal clears are
at each of the arguments. The last argument ends the pedal."
  (let [arguments (list 1 2 4 5)
        shiftarg  (concat (rest arguments) [(last arguments)])
        fromto    (map (fn [ a b ] (list a b)) arguments shiftarg)
        ramps     (sort-by first (mapcat (fn [ [ s e ] ]
                                           (concat
                                            ;; down
                                            (if (= s e) []
                                                (map (fn [v] [ ( + (* v 0.01) s 0.05) (int (* 12.7 v)) ] ) (range 11)))
                                            ;; up
                                            (map (fn [v] [ ( + (* v 0.005) e) (int (* 12.7 (- 10 v))) ] ) (range 11))))
                                         fromto))
        result    (ls/concrete-logical-sequence (map (fn [[b l]] (ls/sustain-pedal-event l b)) ramps))
        ]
    {:composition-type ::sequence :composition-payload result  })
  )

(defn on-instrument [ inst seq ]
  )

(defn with-clock [ clock seq ]
  )

;; this is wrong; the instrument should bind to the sequence (as should, potentially the clock)
(defn midi-play [ sequence on clock ]
  (if (not (= (:composition-type sequence) ::sequence)) (throw (ex-info "I can only midi-play a sequence" { :sequence-was sequence }))
      (let [target  (:composition-payload sequence)
            ps      (-> (ps/new-sequence)
                        (ptol/schedule-logical-on-physical target on clock))]
        (ps/play ps)
        
        )
      )
  )

