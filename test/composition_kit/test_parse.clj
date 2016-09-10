(ns composition-kit.test-parse
  (use clojure.test)
  (require [composition-kit.parse :as p]))

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
  )

;;(run-tests)





