(ns composition-kit.test-parse
  (use clojure.test) 
  (require [composition-kit.parse :as p])
  (require [composition-kit.logical-sequence :as ls]))

(deftest lily-parse
  (let [parse (p/lily->n "a4 bes8 r16 a a'4 ges,," )]
    (is (= (count parse) 6))
    (is (= (map :rest parse) (list false false true false false false)))
    (is (= (:note-name (first parse)) :a3))
    (is (nil? (:note (nth parse 2))))
    (is (= (map :note-name parse) (list :a3 :bes3 nil :a3 :a4 :ges2)))
    (is (= (map :starts-at parse) (list 0 1 3/2 7/4 2 3)))
    (is (= (map :dur parse) (list 1 1/2 1/4 1/4 1 1)))
    )
  (let [parse (p/lily-to-logical-sequence "a4 bes8 r16 a a'4 ges,,")
        by-t  (group-by ls/item-type parse)
        notes (:composition-kit.logical-sequence/notes-with-duration by-t)
        rests (:composition-kit.logical-sequence/rest-with-duration by-t)
        ]
    (is (= (count parse) 6))
    (is (= (count notes) 5))
    (is (= (count rests) 1))
    (is (= (flatten (map #(:notes (ls/item-payload %)) notes)) '(:a3 :bes3 :a3 :a4 :ges2)))
    (is (= (map ls/item-beat parse) '(0 1 3/2 7/4 2 3)))
    (is (= (map ls/item-beat notes) '(0 1 7/4 2 3)))
    (is (= (map ls/item-beat rests) '(3/2)))
    )

  (let [parse (p/lily->n "c4 bes ees4. des8")]
    (is (= (map :starts-at parse) (list 0 1 2 7/2)))
    (is (= (map :dur parse) (list 1 1 3/2 1/2)))
    )

  )

(deftest step-parse
  (let [parse (p/str->n :c2 "X...X...X.X.X...")]
    (is (= (count parse) 16))
    (is (= (count (filter #(= (ls/item-type %) :composition-kit.logical-sequence/notes-with-duration) parse)) 5))
    (is (= (count (filter #(= (ls/item-type %) :composition-kit.logical-sequence/rest-with-duration) parse)) 11))
    (is (= (map ls/item-beat (filter #(= (ls/item-type %) :composition-kit.logical-sequence/notes-with-duration) parse))
           '(0 1 2 5/2 3)))
    (is (= (map (comp :notes ls/item-payload)
                (filter #(= (ls/item-type %) :composition-kit.logical-sequence/notes-with-duration) parse))
           (repeat 5 :c2)))
    )
  (let [parse (p/str->n :c2 "azAZ09~~")
        dyn   (map #(% 0) (map ls/item-dynamics parse))]
    (is (= dyn '(0 127 0 127 0 127 63 63)))
    )
  )
