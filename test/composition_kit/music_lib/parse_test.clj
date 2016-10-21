(ns composition-kit.music-lib.parse-test
  (use clojure.test) 
  (require [composition-kit.music-lib.parse :as p])
  (require [composition-kit.music-lib.logical-sequence :as ls])
  (require [composition-kit.music-lib.logical-item :as i]))

(deftest lily-parse  
  (let [parse (p/lily-to-logical-sequence "a4 bes8 r16 a a'4 ges,,")
        by-t  (group-by i/item-type parse)
        notes (:composition-kit.music-lib.logical-item/notes-with-duration by-t)
        rests (:composition-kit.music-lib.logical-item/rest-with-duration by-t)
        ]
    (is (= (count parse) 6))
    (is (= (count notes) 5))
    (is (= (count rests) 1))
    (is (= (flatten (map #(:notes (i/item-payload %)) notes)) '(:a3 :bes3 :a3 :a4 :ges2)))
    (is (= (map i/item-beat parse) '(0 1 3/2 7/4 2 3)))
    (is (= (map i/item-beat notes) '(0 1 7/4 2 3)))
    (is (= (map i/item-beat rests) '(3/2)))
    )
  (let [parse (p/lily-to-logical-sequence "< c  bes'>4. <c bes'>4 <d e>" )]
    (is (= (count parse) 3))
    (is (= (map i/item-beat parse) '(0 3/2 5/2)))
    )

  (let [parse (p/lily-to-logical-sequence "c4 << { d8 e f4 } \\\\ { c4. d} >> c1")]
    (is (= (count parse) 7))
    (is (= (map i/item-beat parse) '(0 1 1 3/2 2 5/2 4)))
    )
  )

(deftest step-parse
  (let [parse (p/str->n :c2 "X...X...X.X.X...")]
    (is (= (count parse) 16))
    (is (= (count (filter #(= (i/item-type %) :composition-kit.music-lib.logical-item/notes-with-duration) parse)) 5))
    (is (= (count (filter #(= (i/item-type %) :composition-kit.music-lib.logical-item/rest-with-duration) parse)) 11))
    (is (= (map i/item-beat (filter #(= (i/item-type %) :composition-kit.music-lib.logical-item/notes-with-duration) parse))
           '(0 1 2 5/2 3)))
    (is (= (map (comp :notes i/item-payload)
                (filter #(= (i/item-type %) :composition-kit.music-lib.logical-item/notes-with-duration) parse))
           (repeat 5 :c2)))
    )
  (let [parse (p/str->n :c2 "azAZ09~~")
        dyn   (map #(% 0) (map i/item-dynamics parse))]
    (is (= dyn '(0 127 0 127 0 127 63 63)))
    )
  )

