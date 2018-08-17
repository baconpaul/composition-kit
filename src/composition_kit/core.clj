(ns composition-kit.core
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])
  (:require [composition-kit.events.physical-sequence :as ps])
  (:require [composition-kit.music-lib.parse :as parse])
  (:require [composition-kit.music-lib.logical-to-physical :as ltop])
  )



(def <*>
  "The <*> operator plays a set of sequences in parallel, with the zero point of both sequences aligned.
  It is implemented by music-lib.logical-sequence/merge-seqyences
  "
  ls/merge-sequences)

(def >>>
  "The >>> operator plays a set of sequences one after another (in sequence) with the zero point of the
  nth sequence starting at the beat length of the n-1th sequence. It is implemented by concat-sequences."
  ls/concat-sequences)

(defn rest-for [dur]
  "rest-for is a short hand utility to create a sequence with a single rest for a given duration"
  (ls/concrete-logical-sequence [(i/rest-with-duration dur 0)]))

(defn lily [ & arguments ]
  "The lily shorthand parses and creates a logical sequence from the extended lilypond phrase syntax.
For the full set of arguments see parse/lily-to-logical-sequence.
"
  (let [lily-bits     (filter string? arguments)
        lily-string   (apply str (interpose " " lily-bits))
        control-bits  (filter (comp not string?) arguments)
        ]
    (apply parse/lily-to-logical-sequence (concat [ lily-string ] control-bits))
    )
  )

(defn step-strings [ & arguments ]
  "step-strings creates a sequence of beats based on the string step sequencer applied to each note.
This is useful for rhythm patterns. For instance, here's a basic rock beat on midi drums

(step-strings [ :c2   \"X...X...X...X...\"
                :d2   \"..P...P...Q...RW\"
                :fis2 \"R.G.R.G.R.G.R.G.\" ])
"
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
  "midi-play takes a set of sequences with instruments and clocks appropriately assigned and plays them through
the midi subsystem. Arguments like :beat-clock :beat-zer and :beat-end are pretty common."
  (let [target item
        ps      (-> (ps/new-sequence)
                    (as-> s
                        ;;(ltop/schedule-logical-on-physical (:composition-payload item)))]
                        (apply ltop/schedule-logical-on-physical (concat [ s item ] opt-arr))))]
    (ps/play ps :user-stop midi/all-notes-off)
    
    )
  )

(defn midi-play-slaved [ item bus & opt-arr ]
  "midi-play takes a set of sequences with instruments and clocks appropriately assigned and plays them through
the midi subsystem. Arguments like :beat-clock :beat-zer and :beat-end are pretty common."
  (let [target item
        ps      (-> (ps/new-sequence)
                    (as-> s
                        ;;(ltop/schedule-logical-on-physical (:composition-payload item)))]
                        (apply ltop/schedule-logical-on-physical (concat [ s item ] opt-arr))))]
    (ps/play-slaved ps bus :user-stop midi/all-notes-off)
    
    )
  )

