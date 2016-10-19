(ns composition-kit.music-lib.test-logical-item
  (use clojure.test)
  (:require [composition-kit.music-lib.logical-item :as i])
  )


(deftest items-basics
  (let [evt (i/music-event "an event" 123)
        nwd (i/notes-with-duration [ :c4 ] 1.2 4.2)
        rwd (i/rest-with-duration 2 5)]
    (is (i/music-item? evt))
    (is (i/music-item? nwd))
    (is (i/music-item? rwd))

    (is (not (i/item-has-dynamics? evt)))
    (is (not (i/item-has-dynamics? rwd))) 
    (is (i/item-has-dynamics? nwd))
    
    (is (= (i/item-type evt) :composition-kit.music-lib.logical-item/music-event))

    (is (= (i/item-beat evt) 123))
    (is (= (i/item-payload evt) "an event"))
    (is (= (i/item-end-beat evt) 123))
    
    (is (= (i/item-beat nwd) 4.2))
    (is (= (i/item-payload nwd) { :notes [ :c4 ] :dur 1.2 :hold-for 1 }))
    (is (= (i/item-end-beat nwd) (+ 1.2 4.2)))

    (is (= (i/item-beat rwd) 5))
    (is (= (i/item-payload rwd) {:dur 2 }))
    (is (= (i/item-end-beat rwd) (+ 2 5)))
    )
  )

(deftest item-transformer
  (let [evt (i/music-event "an event" 12)
        nwd (i/notes-with-duration [ :c4 ] 1 5)
        rwd (i/rest-with-duration 2 5)
        copy-nwd (i/identity-item-transformer nwd)
        upcase-evt (i/add-transform (i/identity-item-transformer evt) :payload (comp clojure.string/upper-case i/item-payload))

        later-rest (i/add-transform (i/identity-item-transformer rwd) :beat (comp (partial + 2) i/item-beat))
        later-note (i/item-beat-shift nwd 4)
        ]
    (is (= (i/item-beat nwd) (i/item-beat copy-nwd)))
    (is (= (i/item-payload nwd) (i/item-payload copy-nwd)))
    (is (= (i/item-type nwd) (i/item-type copy-nwd)))
    (is (= (:itemtype copy-nwd) :composition-kit.music-lib.logical-item/item-transformer))
    (is (not (= (:itemtype copy-nwd) (i/item-type copy-nwd))))

    (is (= (i/item-payload upcase-evt) "AN EVENT"))

    (is (= (i/item-beat later-rest) 7))

    (is (= (i/item-beat later-note) 9))
    (is (= (i/item-end-beat later-note) 10))

    ;; and an error case
    (is (thrown? clojure.lang.ExceptionInfo
                 (i/add-transform (i/identity-item-transformer evt)
                                  :nonsense 1)))
    )
  )
