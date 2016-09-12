(ns composition-kit.test-logical-sequence
  (use clojure.test)
  (require [composition-kit.logical-sequence :as ls]))

(deftest items-basics
  (let [evt (ls/music-event "an event" 123)
        nwd (ls/notes-with-duration [ :c4 ] 1.2 4.2)]
    (is (ls/music-item? evt))
    (is (ls/music-item? nwd))

    (is (= (ls/item-time evt) 123))
    (is (= (ls/payload evt) "an event"))

    (is (= (ls/item-time nwd) 4.2))
    (is (= (ls/payload nwd) { :notes [ :c4 ] :dur 1.2 }))
    )
  )

(deftest sequences
  (let [cs1 (ls/concrete-logical-sequence
             (map ls/notes-with-duration [ :c4 :d4 :e4 :f4 :g4 ] [ 1 1 1/2 1/2 1 ] [ 0 4 3 2 5 ]))
        cs2 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c4 :d4 ] [ 1 1 ] [ 1/2 4 ] ))
        cs3 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c2 :d2 ] [ 1 1  ] [ 0 6 ] ))
        cs4 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c3 :d3 ] [ 1 1  ] [ 5 7 ] ))
        ]
    (is (every? identity (map (fn [a b] (<  (ls/item-time a) (ls/item-time b))) cs1 (rest cs1))) "Monotonicity in time")
    (is (= (map #(:notes (ls/item-payload %)) cs1) [ :c4 :f4 :e4 :d4 :g4 ]))
    (is (= (map #(:notes (:payload %)) (ls/merged-logical-sequences [cs2 cs3])) [ :c2 :c4 :d4 :d2 ]))
    (is (= (map #(:notes (:payload %)) (ls/merged-logical-sequences [cs2 cs4 cs3])) [ :c2 :c4 :d4 :c3 :d2 :d3 ]))
    
    )
  )


