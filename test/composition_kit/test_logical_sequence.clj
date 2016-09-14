(ns composition-kit.test-logical-sequence
  (use clojure.test)
  (require [composition-kit.logical-sequence :as ls]))

(deftest items-basics
  (let [evt (ls/music-event "an event" 123)
        nwd (ls/notes-with-duration [ :c4 ] 1.2 4.2)
        rwd (ls/rest-with-duration 2 5)]
    (is (ls/music-item? evt))
    (is (ls/music-item? nwd))
    (is (ls/music-item? rwd))

    (is (= (ls/item-beat evt) 123))
    (is (= (ls/item-payload evt) "an event"))

    (is (= (ls/item-beat nwd) 4.2))
    (is (= (ls/item-payload nwd) { :notes [ :c4 ] :dur 1.2 :hold-for 1 }))

    (is (= (ls/item-beat rwd) 5))
    (is (= (ls/item-payload rwd) {:dur 2 }))
    )
  )

(deftest merged-sequences
  (let [cs1 (ls/concrete-logical-sequence
             (map ls/notes-with-duration [ :c4 :d4 :e4 :f4 :g4 ] [ 1 1 1/2 1/2 1 ] [ 0 4 3 2 5 ]))
        cs2 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c4 :d4 ] [ 1 1 ] [ 1/2 4 ] ))
        cs3 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c2 :d2 ] [ 1 1  ] [ 0 6 ] ))
        cs4 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c3 :d3 ] [ 1 1  ] [ 5 7 ] ))
        ]
    (is (every? identity (map (fn [a b] (<  (ls/item-beat a) (ls/item-beat b))) cs1 (rest cs1))) "Monotonicity in time")
    (is (= (map #(:notes (ls/item-payload %)) cs1) [ :c4 :f4 :e4 :d4 :g4 ]))
    (is (= (map #(:notes (:payload %)) (ls/merged-logical-sequences [cs2 cs3])) [ :c2 :c4 :d4 :d2 ]))
    (is (= (map #(:notes (:payload %)) (ls/merged-logical-sequences [cs2 cs4 cs3])) [ :c2 :c4 :d4 :c3 :d2 :d3 ]))
    )
  )

(deftest from-pitch-dur
  (let [mary-had (ls/sequence-from-pitches-and-durations [ :e4 :d4 :c4 :d4 :e4 :e4 :e4 ] [ 1 1 1 1 1 1 2 ] )
        bill-tell (ls/sequence-from-pitches-and-durations
                   [ :c4 :c4 :c4 :c4 :c4 :c4 :c4 :c4 :f4 :g4 :a ]
                   [ 1/2 1/2 1   1/2 1/2 1   1/2 1/2  1  1    1 ])
        legato-mice (ls/sequence-from-pitches-and-durations [ :e4 :d4 :c4 ] [ 1 1 2 ] :length :legato )
        regular-mice (ls/sequence-from-pitches-and-durations [ :e4 :d4 :c4 ] [ 1 1 2 ] )
        staccato-mice (ls/sequence-from-pitches-and-durations [ :e4 :d4 :c4 ] [ 1 1 2 ] :length :staccato )
        delayed-mice (ls/sequence-from-pitches-and-durations [ :e4 :d4 :c4 ] [ 1 1 2 ] :start-beat 3 )
        ]
    (is (= (count mary-had) 7))
    (is (ls/music-item? (first mary-had)))
    (is (= (:itemtype (first mary-had)) :composition-kit.logical-sequence/notes-with-duration))
    (is (= (:payload (first mary-had)) { :notes :e4 :dur 1 :hold-for 0.95 }))
    (is (= (map :beat mary-had) (list 0 1 2 3 4 5 6)))

    (is (= (map :beat bill-tell) (list 0 1/2 1 2 5/2 3 4 9/2 5 6 7 )))

    (is (= (count legato-mice) 3))
    (is (= (count staccato-mice) 3))
    (is (= (count regular-mice) 3))
    (is (= (count delayed-mice) 3))

    (is (every? identity (map (fn [a b c] (> (:hold-for (ls/item-payload a))
                                             (:hold-for (ls/item-payload b))
                                             (:hold-for (ls/item-payload c))))
                              legato-mice regular-mice staccato-mice)))

    (is (= (map :beat delayed-mice) (list 3 4 5)))
    )
  )

(deftest loops
  (let [phrase  (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 :f4 :e4 :f4 :g4 :c4 ] [ 1 1 1/4 1/4 1/4 1/4 1/2 1/2 ])
        phrase-short (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 ] [ 1 1/2 1/2 ] )]
    (is (= (count phrase) 8))
    (is (= (reduce + (map (comp :dur ls/item-payload) phrase)) 4))
    (is (= (ls/loop-sequence phrase 1) phrase))
    (is (= (count (ls/loop-sequence phrase 2)) 16))
    (let [loop-4 (ls/loop-sequence phrase 4)]
      (is (= (count loop-4) (* (count phrase) 4)))
      (is (every? identity (map (fn [a b] (<  (ls/item-beat a) (ls/item-beat b))) loop-4 (rest loop-4))) "Monotonicity in time")
      )
    (let [loop-3 (ls/loop-sequence phrase-short 3)]
      (is (= (count loop-3) (* 3 (count phrase-short))))
      (is (= (map (comp :notes ls/item-payload) loop-3) (list :c4 :d4 :e4 :c4 :d4 :e4 :c4 :d4 :e4)))
      (is (= (map (comp :dur ls/item-payload) loop-3 ) (list 1 1/2 1/2 1 1/2 1/2 1 1/2 1/2)))
      (is (= (map ls/item-beat loop-3) (list 0 1 3/2 2 3 7/2 4 5 11/2 )))
      )
    ))

