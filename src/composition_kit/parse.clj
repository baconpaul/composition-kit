(ns composition-kit.parse
  (require [composition-kit.tonal-theory :as th]))

;; rejigger this so it works

(defn ^:private lily-note-to-data [n prior]
  (let [[m1 pitch prel pdur]   (re-find (re-pattern "^([a-z]+)([^\\d]*)(\\d*.*)") n)
        ]
        )


(defn lily->n
  "Given a subset of the lilypond melody format generate a data structure
  which play ascii can play as notes. For instance
   :bassline   (lily->n  \"a4 b8 a c4 a'4 r8 a,8\" :relative \"C2\")"
  ([line & optsarr]
   (let [opt    (apply hash-map optsarr)
         notes  (clojure.string/split line #" ")
         rel    (or (:relative opt) "c4")
         fprior { :note (th/notes)
     (map #(dissoc % :prior)
          (loop  [n    notes
                  res  []
                  p    fprior ]
            (if (empty? n)
              res
              (let [ curr  (lily-note-to-data (first n) p) ]
                (recur (rest n) (conj res curr) curr))))))))
