(ns composition-kit.music-lib.tempo-test
  (use clojure.test)
  (require [composition-kit.music-lib.tempo :as t]))

(deftest test-constant-tempo
  (let [c1  (t/constant-tempo 3 4 150)
        c2  (t/constant-tempo 3 4 75)
        c3  (t/constant-tempo 4 4 150)
        csecond (t/constant-tempo 1 4 60)]
    (is (t/metronome? c1))
    (is (t/metronome? c2))
    (is (t/metronome? c3))
    (is (t/metronome? csecond))
    
    (is (zero? (t/beats-to-time c1 0)))
    (is (= (t/beats-to-time c1 37) (t/beats-to-time c3 37)))
    (is (= (t/beats-to-time c1 38) (t/beats-to-time c2 (/ 38 2))))
    (is (= (t/beats-to-time c1 38) (/ (t/beats-to-time c2 38) 2)))

    ;; Remember measures-beat is a one-based system but absolute beats are a zero-based system
    (is (= (t/measure-beat-to-beat c1 1 1) 0))
    (is (= (t/measure-beat-to-beat c2 1 1) 0))
    (is (= (t/measure-beat-to-beat c3 1 1) 0))
    
    (is (= (t/measure-beat-to-beat c1 4 1) 9))
    (is (= (t/measure-beat-to-beat c3 4 1) 12))
    (is (= (t/measure-beat-to-beat c1 5 1) (t/measure-beat-to-beat c3 4 1)))

    (is (= (t/beats-to-time csecond 40) 40))
    )
  )

(deftest test-multi-segment-tempi
  (let [c1 (t/multi-segment-constant-tempi 4 4
                                           1 120
                                           11 60
                                           21 90)]
    (is (t/metronome? c1))
    (is (zero? (t/beats-to-time c1 0)))

    ;; OK so measure-beat-to-beat should all still be normal since it's a fixed metric
    (is (= (t/measure-beat-to-beat c1 1 1) 0))
    (is (= (t/measure-beat-to-beat c1 2 2) 5))
    (is (= (t/measure-beat-to-beat c1 11 1) 40))
    (is (= (t/measure-beat-to-beat c1 51 1) 200))

    ;; but measure-beat-to-time should change when we fall over points
    (is (= (t/measure-beat-to-time c1 1 1) 0))
    (is (= (t/measure-beat-to-time c1 2 1) 2))
    (is (= (t/measure-beat-to-time c1 11 1) 20))
    (is (= (t/measure-beat-to-time c1 12 1) 24))
    (is (= (t/measure-beat-to-time c1 21 1) 60))
    (is (= (t/measure-beat-to-time c1 21 2) (+ 60 2/3)))
    )
  )


(deftest wonky-parameters
  (is (= (try (t/multi-segment-constant-tempi 3 4  0 80  2 83) (catch Exception e (.getMessage e))) "The first measure has to be 1"))
  )

