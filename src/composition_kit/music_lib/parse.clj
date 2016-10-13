(ns composition-kit.music-lib.parse
  (require [composition-kit.music-lib.logical-sequence :as ls])
  (require [composition-kit.music-lib.tonal-theory :as th]))


(defn ^:private lily-note-to-data [n prior]
  (let [[m1 spitch prel pdur]   (re-find (re-pattern "^([a-z]+)([^\\d]*)(\\d*.*)") n)
        
        ;; Duration carries over from prior if not specified
        ;; FIXME: We need to deal with duration parsing (like 4.. and stuff) and have to worry
        ;; about triplets one day
        sdur                   (if (= pdur "") (:sdur prior) pdur)
        [m ndur dots]          (re-find (re-pattern "^(\\d+)(\\.*)") sdur)
        dur                    (* (/ 4 (Integer/parseInt ndur)) (reduce + 1 (take (count dots) (iterate #(/ % 2) 1/2))))

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
    (map
     (fn [ln]
       (if (:rest ln) (ls/rest-with-duration (:dur ln) (:starts-at ln))
           (ls/notes-with-duration [ (:note (:note ln)) ] (:dur ln) (:starts-at ln))))
     (sort-by :starts-at notes))))


(defn char-to-range
  "given a character make a midi range with semantic that 0-9 and
  a-z both map to min to max accordingly"
  ([c]  (char-to-range c 0 127))
  ([c min max]
   (let  [within  (fn [l h] (and (>= 0 (compare l c)) (>= 0 (compare c h))))
          spread  (* 1.0 (- max min))
          val     (fn [z r]
                    (let [cz   (- (int c) (int z))
                          delt (/ spread (dec r))]
                      (int (+ (* cz delt) min))))
          ]
     (cond
       (within \0 \9) (val \0 10)
       (within \a \z) (val \a 26)
       (within \A \Z) (val \A 26)
       true           (int (+ (/ spread 2) min))))))

(defn str->n
  "Given a single line of a sequence like

   'X...X...X.X.X...'

  and a note name (like :c1) return a logical sequence with the pattern inserted.

  Additional arguments include  
    :dur   (to have a different note duration; the default is 1/16)
    :xform (to have a different char-to-range)
    :item-factory (to make something other than notes with duration)
  
  (str->n :c2 \"x...a...x...y.z.\" :dur 1/32) for 32n/d notes
  "
  ([target line & restp]
   (let [restmap  (apply hash-map restp)
         xform    (or (:xform restmap) char-to-range)
         dur      (or (:dur restmap) 1/4)] ;; 1/4 of a beat is a 16th note generally
     (->>
      (map-indexed (fn[idx c] {:char      c
                               :note     (not (= c \.))
                               :value    (if (= c \.) 0 (xform c))
                               :dur      dur
                               :beat     (* idx dur)
                               })
                   (clojure.string/trim line))
      ;;(filter :note)
      (map (fn [itm]
             (if (:note itm)
               (ls/add-transform
                (ls/identity-item-transformer
                 (ls/notes-with-duration target (:dur itm) (:beat itm)))
                :dynamics
                (constantly (constantly (:value itm))))
               (ls/rest-with-duration (:dur itm) (:beat itm))
               )))
      (ls/concrete-logical-sequence)
      ))))




