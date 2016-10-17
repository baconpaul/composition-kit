(ns composition-kit.core
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.events.physical-sequence :as ps])
  (:require [composition-kit.music-lib.parse :as parse])
  (:require [composition-kit.music-lib.logical-to-physical :as ltop])
  )


(defn lift-to-seq [item f & args ]
  "basically return the sequence with f (orig) args repacing the sequence"
  {:composition-type (:composition-type item)
   :composition-payload (apply f (cons (:composition-payload item) args))})

(defn phrase [ & arguments ]
  (let [by-type  (group-by :composition-type arguments)
        types    (set (keys by-type))]
    {:composition-type ::sequence
     :composition-payload 
     (as-> (ls/concrete-logical-sequence []) it
       
       ;; So do we have any sequences? If so they win. Clobber 'it' with their value
       (if (::sequence types) (apply ls/merge-sequences (map :composition-payload (::sequence by-type)))
           ;; else
           it)

       ;; Do we have a pitch or a duration?
       (if (or (::pitches types) (::durations types))
         (if (and (= 1
                     (count (::pitches by-type))
                     (count (::durations by-type))))
           (ls/merge-sequences it (ls/sequence-from-pitches-and-durations
                                   (:composition-payload (first (::pitches by-type)))
                                   (:composition-payload (first (::durations by-type)))))
           (throw (ex-info "One and only one pitch and duration statement allowed in a phrase, sorry", { ::pitches (::pitches by-type)
                                                                                                        ::durations (::durations by-type)})))
         ;; else         
         it)

       ;; Do we have any dynamics
       (if  (::dynamics types)
         (do
           (when (> (count (::dynamics by-type)) 1)
             (throw (ex-info "One and only one dynamics in a phrase, sorry", { ::dynamics (::dynamics by-type)})))
           (let [ dyn  (first (::dynamics by-type)) ]
             (condp = (:dynamics-type dyn)
               ::explicit
               (ls/explicit-segment-dynamics it (:composition-payload dyn))
               ::function
               ((:composition-payload dyn) it)
               )
             ))
         ;; else
         it)
       
       )
     }
    
    )
  )

(defn raw-sequence [ s]
  {:composition-type ::sequence :composition-payload s})

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

(defn overlay [ & arguments ]
  (if (not (every? #(= (:composition-type %) ::sequence) arguments))
    (throw (ex-info "Can only overlay sequences. You handed me these types"
                    { :types (map :composition-type arguments) :args arguments }))
    {:composition-type ::sequence :composition-payload (apply ls/merge-sequences (map :composition-payload arguments))}
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
    (apply overlay
           (map (fn [[note str]]
                  (raw-sequence (parse/str->n note str))) note-str-pairs))
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
  (let [arg-group (partition 3 arguments)
        _         (when-not (every? (fn [ [ b s l ] ] (and (= (type l) java.lang.Long)  (= s '->))) arg-group)
                    (throw (ex-info "Incorrect syntax. Should be b -> l b -> l. You gave me arguments as shown." { :args arguments } )))
        line-seg  (mapcat (fn [ [ b s l ] ] [ b l ] ) arg-group)
        argname   (gensym 'seq_)]
    `{:composition-type ::dynamics :dynamics-type ::function :composition-payload
      (fn [ ~argname ] (ls/line-segment-dynamics ~argname ~@line-seg))}
    )
  )

(defn hold-for [seqn amt]
  (lift-to-seq seqn
               ls/apply-note-payload-transform
               (fn [i p] (assoc p :hold-for amt))))

(defn transform-note-payload [seqn f]
  (lift-to-seq seqn
               ls/apply-note-payload-transform f))


(defn concatenate [ & arguments ]
  (if (not (every? #(= (:composition-type %) ::sequence) arguments))
    (throw (ex-info "Can only conctenate sequences. You handed me these types"
                    { :types (map :composition-type arguments) }))
    {:composition-type ::sequence :composition-payload (apply ls/concat-sequences (map :composition-payload arguments))}
    )
  )

(defn pedal-held-and-cleared-at [ & arguments ]
  "Given a collection of beats, depress the pedal just a smidge after the beat and then
hold it until the next beat, where it releases and re-applies. So basically pedal clears are
at each of the arguments. The last argument ends the pedal."
  (let [shiftarg  (concat (rest arguments) [(last arguments)])
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

(defn on-instrument [ seq inst ]
  (lift-to-seq seq ls/assign-instrument inst)
  )

(defn with-clock [ seq clock ]
  (lift-to-seq seq ls/assign-clock clock)
  )

(defn loop-n [ seq count ]
  (lift-to-seq seq ls/loop-sequence count)
  )

;; this is wrong; the instrument should bind to the sequence (as should, potentially the clock)
(defn midi-play [ item & opt-arr ]
  (cond
    (= (:composition-type item) ::sequence)
    (let [target  (:composition-payload item)
          ps      (-> (ps/new-sequence)
                      (as-> s
                          ;;(ltop/schedule-logical-on-physical (:composition-payload item)))]
                          (apply ltop/schedule-logical-on-physical (concat [ s (:composition-payload item) ] opt-arr))))]
      (ps/play ps)
      
      )
    :else (throw (ex-info "Can't midi-play type. Make a sequence please." {:type (:composition-type item)}))
    )
  )

