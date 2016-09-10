(ns composition-kit.test-music-sequence
  (use clojure.test)
  (require [composition-kit.music-sequence :as ms])
  )

;; Run a set of functions recording return values and see how they do
(deftest play-sequence-test
  (let [test-set (-> (ms/new-sequence)
                     (ms/add-to-sequence (fn [t] {:dt t :ct (System/currentTimeMillis)}) 500)
                     (ms/add-to-sequence (fn [t] {:dt t :ct (System/currentTimeMillis)}) 1000)
                     (ms/add-to-sequence (fn [t] {:dt t :ct (System/currentTimeMillis)}) 2000)
                     (ms/add-to-sequence (fn [t] {:dt t :ct (System/currentTimeMillis)}) 2600)
                     (ms/play)
                     ;; Now if we wanted this to be a production app we would add a watcher looking
                     ;; for the play status to flip to false or seq to empty but instead do this
                     ( (fn [a] (Thread/sleep 3000 ) @a))
                     :return-values
                     (#(map (fn [p n] (reduce (fn [s k] (assoc s k (- (k n) (k p)))) {} (keys p))) % (rest %)))
                     )
        close   (fn [a b] (< (max (- a b) (- b a)) 2))
        ]
    (is (every? identity (map #( close (:dt %) (:ct %)) test-set)))
    (is (every? identity (map close (map :dt test-set) '(500 1000 600))))
    )
  )

(deftest assemble-sequence-test
  ;; Lots of different ways to assemble sequences
  (let [s1   (-> (ms/new-sequence)
                 (ms/add-to-sequence identity 0 identity 100 identity 200 identity 400))
        ;; order shouldn't matter
        s2   (-> (ms/new-sequence)
                 (ms/add-to-sequence identity 100 identity 0 identity 400 identity 200))
        ;; nor syntax
        s3   (-> (ms/new-sequence)
                 (ms/add-to-sequence identity 100)
                 (ms/add-to-sequence identity 0)
                 (ms/add-to-sequence identity 200)
                 (ms/add-to-sequence identity 400))
        stimes (fn [s] (map :time (:seq s)))
        disitem (fn [s] (distinct (map :item (:seq s))))
        ]
    (is (= (stimes s1) (stimes s2)))
    (is (= (stimes s1) (stimes s3)))

    (is (= (count (disitem s1)) 1))
    (is (= (count (disitem s2)) 1))
    (is (= (count (disitem s3)) 1))

    (is (= (first (disitem s1)) identity))
    (is (= (first (disitem s2)) identity))
    (is (= (first (disitem s3)) identity))

    (is (:play s1))
    (is (:play s2))
    (is (:play s3))
    )
  )


;; (run-tests)


