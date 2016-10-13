(ns composition-kit.music-lib.test-logical-sequence
  (use clojure.test)
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.midi-util :as midi])
  )

(deftest items-basics
  (let [evt (ls/music-event "an event" 123)
        nwd (ls/notes-with-duration [ :c4 ] 1.2 4.2)
        rwd (ls/rest-with-duration 2 5)]
    (is (ls/music-item? evt))
    (is (ls/music-item? nwd))
    (is (ls/music-item? rwd))

    (is (not (ls/item-has-dynamics? evt)))
    (is (not (ls/item-has-dynamics? rwd))) 
    (is (ls/item-has-dynamics? nwd))
    
    (is (= (ls/item-type evt) :composition-kit.music-lib.logical-sequence/music-event))

    (is (= (ls/item-beat evt) 123))
    (is (= (ls/item-payload evt) "an event"))
    (is (= (ls/item-end-beat evt) 123))
    
    (is (= (ls/item-beat nwd) 4.2))
    (is (= (ls/item-payload nwd) { :notes [ :c4 ] :dur 1.2 :hold-for 1 }))
    (is (= (ls/item-end-beat nwd) (+ 1.2 4.2)))

    (is (= (ls/item-beat rwd) 5))
    (is (= (ls/item-payload rwd) {:dur 2 }))
    (is (= (ls/item-end-beat rwd) (+ 2 5)))
    )
  )

(deftest item-transformer
  (let [evt (ls/music-event "an event" 12)
        nwd (ls/notes-with-duration [ :c4 ] 1 5)
        rwd (ls/rest-with-duration 2 5)
        copy-nwd (ls/identity-item-transformer nwd)
        upcase-evt (ls/add-transform (ls/identity-item-transformer evt) :payload (comp clojure.string/upper-case ls/item-payload))

        later-rest (ls/add-transform (ls/identity-item-transformer rwd) :beat (comp (partial + 2) ls/item-beat))
        later-note (ls/item-beat-shift nwd 4)
        ]
    (is (= (ls/item-beat nwd) (ls/item-beat copy-nwd)))
    (is (= (ls/item-payload nwd) (ls/item-payload copy-nwd)))
    (is (= (ls/item-type nwd) (ls/item-type copy-nwd)))
    (is (= (:itemtype copy-nwd) :composition-kit.music-lib.logical-sequence/item-transformer))
    (is (not (= (:itemtype copy-nwd) (ls/item-type copy-nwd))))

    (is (= (ls/item-payload upcase-evt) "AN EVENT"))

    (is (= (ls/item-beat later-rest) 7))

    (is (= (ls/item-beat later-note) 9))
    (is (= (ls/item-end-beat later-note) 10))

    ;; and an error case
    (is (thrown? clojure.lang.ExceptionInfo
                 (ls/add-transform (ls/identity-item-transformer evt)
                                   :nonsense 1)))
    )
  )

(deftest merge-sequences
  (let [cs1 (ls/concrete-logical-sequence
             (map ls/notes-with-duration [ :c4 :d4 :e4 :f4 :g4 ] [ 1 1 1/2 1/2 1 ] [ 0 4 3 2 5 ]))
        cs2 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c4 :d4 ] [ 1 1 ] [ 1/2 4 ] ))
        cs3 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c2 :d2 ] [ 1 1  ] [ 0 6 ] ))
        cs4 (ls/concrete-logical-sequence (map ls/notes-with-duration [ :c3 :d3 ] [ 1 1  ] [ 5 7 ] ))
        ]
    (is (= (ls/beat-length cs1) 6))
    (is (every? identity (map (fn [a b] (<  (ls/item-beat a) (ls/item-beat b))) cs1 (rest cs1))) "Monotonicity in time")
    (is (= (map #(:notes (ls/item-payload %)) cs1) [ :c4 :f4 :e4 :d4 :g4 ]))
    (is (= (map #(:notes (ls/item-payload %)) (ls/merge-sequences cs2 cs3)) [ :c2 :c4 :d4 :d2 ]))
    (is (= (map #(:notes (ls/item-payload %)) (ls/merge-sequences cs2 cs4 cs3)) [ :c2 :c4 :d4 :c3 :d2 :d3 ]))
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

        boring  (ls/sequence-from-pitches-constant-duration [ :c4 :e4 :c4 :e4 ] 1/2 :length :legato )
        very-boring (ls/repeated-note :c4 1/2 4 )
        ]
    (is (= (count mary-had) 7))
    (is (= (ls/beat-length mary-had) 8))
    (is (ls/music-item? (first mary-had)))
    (is (= (ls/item-type (first mary-had)) :composition-kit.music-lib.logical-sequence/notes-with-duration))
    (is (= (ls/item-payload (first mary-had)) { :notes :e4 :dur 1 :hold-for 0.95 }))
    (is (= (ls/item-payload (last mary-had)) { :notes :e4 :dur 2 :hold-for 1.9 }))
    (is (= (map ls/item-beat mary-had) (list 0 1 2 3 4 5 6)))
    (is (= (map ls/item-beat bill-tell) (list 0 1/2 1 2 5/2 3 4 9/2 5 6 7 )))

    (is (= (map ls/item-beat boring) (list 0 1/2 1 3/2)))
    (is (= (:hold-for (ls/item-payload (first boring)))  1/2))

    (is (= (map ls/item-beat very-boring) (list 0 1/2 1 3/2)))

    (is (= (count legato-mice) 3))
    (is (= (count staccato-mice) 3))
    (is (= (count regular-mice) 3))
    (is (= (count delayed-mice) 3))

    (is (every? identity (map (fn [a b c] (> (:hold-for (ls/item-payload a))
                                             (:hold-for (ls/item-payload b))
                                             (:hold-for (ls/item-payload c))))
                              legato-mice regular-mice staccato-mice)))

    (is (= (map ls/item-beat delayed-mice) (list 3 4 5)))
    )
  )


(deftest loops
  (let [phrase  (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 :f4 :e4 :f4 :g4 :c4 ] [ 1 1 1/4 1/4 1/4 1/4 1/2 1/2 ])
        phrase-short (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 ] [ 1 1/2 1/2 ] )]
    (is (= (count phrase) 8))
    (is (= (reduce + (map (comp :dur ls/item-payload) phrase)) 4))
    (is (= (map ls/item-payload (ls/loop-sequence phrase 1)) (map ls/item-payload phrase)))
    (is (= (map ls/item-beat (ls/loop-sequence phrase 1)) (map ls/item-beat phrase)))
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

(deftest length-tests
  (let [mary   (ls/sequence-from-pitches-and-durations [ :e4 :d4 :c4 :d4 :e4 :e4 :e4 ] [ 1 1 1 1 1 1 2 ] )
        harm   (ls/sequence-from-pitches-and-durations [ :c2 :g2 :c2 ] [ 2 2 4 ] )
        song   (ls/merge-sequences mary harm)]
    (is (= (ls/beat-length mary) (ls/beat-length harm) 8))
    (is (= (ls/beat-length song) 8))
    ))

(deftest shifts
  (let [phrase-short (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 ] [ 1 1/2 1/2 ] )
        one-later    (ls/beat-shift phrase-short 1)
        zero-later   (ls/beat-shift phrase-short 0)
        five-later   (ls/beat-shift phrase-short 5)
        ]
    (is (= (count phrase-short) (count one-later) (count zero-later) (count five-later)))
    (is (= (ls/beat-length phrase-short)
           (ls/beat-length one-later)
           (ls/beat-length zero-later)
           (ls/beat-length five-later)))
    (is (= (ls/beat-length-from-zero phrase-short) 2))
    (is (= (ls/beat-length-from-zero five-later) 7))
    (is (= (map ls/item-beat phrase-short) [ 0 1 3/2 ]))
    (is (= (map ls/item-beat zero-later) (map ls/item-beat phrase-short)))
    (is (every? (partial = 1) (map - (map ls/item-beat one-later) (map ls/item-beat phrase-short))))
    (is (every? (partial = 5) (map - (map ls/item-beat five-later) (map ls/item-beat phrase-short))))

    )
  )


(deftest concat-tests
  (let [mary   (ls/sequence-from-pitches-and-durations [ :e4 :d4 :c4 :d4 :e4 :e4 :e4 ] [ 1 1 1 1 1 1 2 ] )
        harm   (ls/sequence-from-pitches-and-durations [ :c2 :g2 :c2 ] [ 2 2 4 ] )
        song   (ls/merge-sequences mary harm)]
    (is (= (map ls/item-beat mary) [ 0 1 2 3 4 5 6 ] ))
    (is (= (map ls/item-beat song) [ 0 0 1 2 2 3 4 4 5 6 ]))
    (is (= (map ls/item-beat (ls/concat-sequences mary mary)) [ 0 1 2 3 4 5 6 8 9 10 11 12 13 14 ] ))
    (is (= (map ls/item-beat (ls/concat-sequences song song))
           [ 0 0 1 2 2 3 4 4 5 6 8 8 9 10 10 11 12 12 13 14 ] ))
    ))


(deftest note-dynamics
  (let [note      (ls/notes-with-duration [ :c4 ] 1 1)
        note2     (ls/notes-with-duration [ :c4 ] 1 4)
        loud-note (ls/override-dynamics note (constantly 127))
        soft-note (ls/constant-dynamics note 20)
        
        louder-later (fn [it] (apply min [ 127 (* 10 (ls/item-beat it))])) 
        f-note    (ls/override-dynamics note louder-later)
        f-note2   (ls/override-dynamics note2 louder-later)
        
        volume-up (fn [it dyn] (+ 5 dyn)) 
        l-note    (ls/compose-dynamics note volume-up)

        ll-note   (ls/louder-by note 10)
        ss-note   (ls/softer-by note 10)
        ]
    (is (ls/item-has-dynamics? note))
    (is (= (ls/note-dynamics-to-7-bit-volume note) 80))
    (is (= (ls/note-dynamics-to-7-bit-volume loud-note) 127))
    (is (= (ls/note-dynamics-to-7-bit-volume soft-note) 20))
    (is (= (ls/note-dynamics-to-7-bit-volume f-note) 10))
    (is (= (ls/note-dynamics-to-7-bit-volume f-note2) 40))
    (is (= (ls/note-dynamics-to-7-bit-volume l-note) 85))
    (is (= (ls/note-dynamics-to-7-bit-volume ll-note) 90))
    (is (= (ls/note-dynamics-to-7-bit-volume ss-note) 70))
    )
  )

(deftest sequence-dynamics
  (let [cs    (ls/repeated-note :c4 1/4 33)
        loud  (ls/override-sequence-dynamics cs (constantly 127))
        swell (ls/line-segment-dynamics cs 0 40 4 80 8 40)

        ds    (ls/repeated-note :d4 1/4 4)
        updn  (ls/explicit-segment-dynamics ds '(127 23 78 42))
        updn2 (ls/explicit-segment-dynamics ds '(127 23)) ;; the 23 should continue
        dyn   (fn [s] (map ls/note-dynamics-to-7-bit-volume s))
        ]
    (is (= (count (dyn cs)) 33))
    (is (= (distinct (dyn cs)) '(80)))
    (is (= (count (dyn loud)) 33))
    (is (= (distinct (dyn loud)) '(127)))
    (is (= (first (dyn swell)) 40))
    (is (= (last (dyn swell)) 40))
    (is (= (apply max (dyn swell)) 80))
    (is (= (second (dyn swell)) 42))
    (is (= (take 4 (dyn swell)) '(40 42 45 47)))

    (is (= (count updn) 4))
    (is (= (count updn2) 4))

    (is (= (dyn updn) '(127 23 78 42)))
    (is (= (dyn updn2) '(127 23 23 23)))
    )
  )

(deftest inst-and-clock
  (let [ph     (ls/repeated-note :c4 1/4 5)
        inst   (midi/midi-instrument 0)
        clock  (tempo/constant-tempo 2 4 140)
        phin   (ls/assign-instrument ph inst)
        phcl   (ls/assign-clock ph clock)
        phall  (-> ph
                   (ls/assign-clock clock)
                   (ls/assign-instrument inst))

        not-nil? (comp not nil?)
        ]
    (is (every? nil? (map ls/item-instrument ph)))
    (is (every? nil? (map ls/item-clock  ph)))

    (is (every? not-nil? (map ls/item-instrument phin)))
    (is (every? nil? (map ls/item-clock  phin)))

    (is (every? nil? (map ls/item-instrument phcl)))
    (is (every? not-nil? (map ls/item-clock  phcl)))

    (is (every? not-nil? (map ls/item-instrument phall)))
    (is (every? not-nil? (map ls/item-clock phall)))
    )
  )

(deftest gotcha-edgecases
  "A collection of edge cases I didn't catch in the sort of core cases above"
  (let [c1  (ls/concrete-logical-sequence [])
        c2  (ls/repeated-note :c4 1/4 4)
        c3  (ls/merge-sequences c1 c2)
        c4  (ls/merge-sequences c1 c1)
        c5  (ls/merge-sequences c2)
        c6  (ls/merge-sequences c1)]
    (is (zero? (count c1)))
    (is (= (count c2) (count c3)))
    (is (zero? (count c4)))
    (is (zero? (count c6)))
    (is (= c2 c5))
    (is (zero? (ls/beat-length-from-zero c1)))
    (is (zero? (ls/beat-length c1)))
    )
  )

