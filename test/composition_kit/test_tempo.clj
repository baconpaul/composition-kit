(ns composition-kit.test-tempo
  (use clojure.test)
  (require [composition-kit.tempo :as t]))

(deftest constant-tempo
  (let [c1  (t/constant-tempo 3 4 150)
        c2  (t/constant-tempo 3 4 75)
        c3  (t/constant-tempo 4 4 150)
        csecond (t/constant-tempo 1 4 60)]
    (is (zero? (t/beats-to-time c1 0)))
    (is (= (t/beats-to-time c1 37) (t/beats-to-time c3 37)))
    (is (= (t/beats-to-time c1 38) (t/beats-to-time c2 (/ 38 2))))
    (is (= (t/beats-to-time c1 38) (/ (t/beats-to-time c2 38) 2)))

    (is (= (t/measure-beat-to-beat c1 4 1) 12))
    (is (= (t/measure-beat-to-beat c3 4 1) 16))
    (is (= (t/measure-beat-to-beat c1 5 3) (t/measure-beat-to-beat c3 4 2)))

    (is (= (t/beats-to-time csecond 40) 40))
    )
  )





