(ns composition-kit.music-sequence)

(defn ^:private compare-by-key-then [k]
  (fn [c1 c2]
    (let [comp  (compare (k c1) (k c2))]
      (if (zero? comp)
        (compare c1 c2)
        comp)))
  )

(def seq-atom
