(ns composition-kit.parse
  (require [composition-kit.logical-sequence :as ls])
  (require [composition-kit.tonal-theory :as th]))


(defn ^:private lily-note-to-data [n prior]
  (let [[m1 spitch prel pdur]   (re-find (re-pattern "^([a-z]+)([^\\d]*)(\\d*.*)") n)
        
        ;; Duration carries over from prior if not specified
        ;; FIXME: We need to deal with duration parsing (like 4.. and stuff) and have to worry
        ;; about triplets one day
        sdur                   (if (= pdur "") (:sdur prior) pdur)
        dur                    (/ 4 (Integer/parseInt sdur)) ;; in quarter note beats

        rest                   (if (= spitch "r") true false)
        pitch                  (if rest (:pitch prior) (keyword spitch))

        prior-oct              (:octave (:note prior))
        prior-pitch            (or (:pitch (:note prior)) (:pitch  prior))

        ninterval              (let [i (th/interval-between prior-pitch pitch)]
                                 (if (<= i 7) i (- i 12)))

        starts-at              (or (:ends-at prior) 0)
        
        roctavediff            (cond
                                 (= prel ",") -1
                                 (= prel ",,") -2
                                 (= prel ",,,") -3
                                 (= prel ",,,,") -4
                                 (= prel "'" ) 1
                                 (= prel "''" ) 2
                                 (= prel "'''" ) 3
                                 (= prel "''''" ) 4
                                 true 0)

        interval               ( + ( * 12 roctavediff) ninterval )
        notes                  (th/notes-by-midinote (+ interval (:midinote (:note prior))))
        note                   (first (filter #(= (:pitch %) pitch) notes))
        ]
    {
     :lily  n
     :pitch pitch
     :dur   dur
     :sdur  sdur
     :interval interval
     :prior (dissoc prior :prior)
     :note note
     :rest rest
     :starts-at  starts-at
     :ends-at    (+ dur starts-at)
     
     }
    ))


(defn lily->n
  "Given a subset of the lilypond melody format generate a data structure
  which play ascii can play as notes. For instance
   :bassline   (lily->n  \"a4 b8 a c4 a'4 r8 a,8\" :relative :c4)"
  ([line & optsarr]
   (let [opt    (apply hash-map optsarr)
         notes  (clojure.string/split line #" ")
         rel    (or (:relative opt) :c4)
         fprior { :note (th/note-by-name rel) }]
     (map (fn[ el ]
            (-> (dissoc el :prior)
                ((fn [x] (assoc x :note (when-not (:rest x) (:note x)))))
                ((fn [x] (assoc x :note-name (:note (:note x)))))))
          (loop  [n    notes
                  res  []
                  p    fprior ]
            (if (empty? n)
              res
              (let [ curr  (lily-note-to-data (first n) p) ]
                (recur (rest n) (conj res curr) curr)))))))
  )

(defn lily-to-logical-sequence
  [line & optarr ]
  (let [notes  (apply lily->n (concat [line] optarr))]
    (map (fn [ln] (ls/notes-with-duration [ (:note (:note ln)) ] (:dur ln) (:starts-at ln)))
         (sort-by :starts-at (filter (comp not :rest) notes)))))
