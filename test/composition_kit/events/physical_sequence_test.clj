(ns composition-kit.events.physical-sequence-test
  (use clojure.test)
  (require [composition-kit.events.physical-sequence :as ps])
  )

;; Run a set of functions recording return values and see how they do
(deftest play-sequence-test
  (let [test-set (-> (ps/new-sequence)
                     (ps/add-to-sequence (fn [t] {:dt t :ct (System/currentTimeMillis)}) 500)
                     (ps/add-to-sequence (fn [t] {:dt t :ct (System/currentTimeMillis)}) 1000)
                     (ps/add-to-sequence (fn [t] {:dt t :ct (System/currentTimeMillis)}) 2000)
                     (ps/add-to-sequence (fn [t] {:dt t :ct (System/currentTimeMillis)}) 2600)
                     (ps/play)
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
  (let [s1   (-> (ps/new-sequence)
                 (ps/add-to-sequence identity 0 identity 100 identity 200 identity 400))
        ;; order shouldn't matter
        s2   (-> (ps/new-sequence)
                 (ps/add-to-sequence identity 100 identity 0 identity 400 identity 200))
        ;; nor syntax
        s3   (-> (ps/new-sequence)
                 (ps/add-to-sequence identity 100)
                 (ps/add-to-sequence identity 0)
                 (ps/add-to-sequence identity 200)
                 (ps/add-to-sequence identity 400))
        ;; Nor contemperaneous events (as long as they are actually distinct)
        s4   (-> (ps/new-sequence)
                 (ps/add-to-sequence identity 10)
                 (ps/add-to-sequence partial 10)
                 (ps/add-to-sequence juxt 10))

        s5   (try (-> (ps/new-sequence)
                      (ps/add-to-sequence identity))
                  (catch Exception e e))
        
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

    (is (= (count s4) 3))
    (is (= (count (distinct (map :time (:seq s4)))) 1))
    (is (= (set (map :item (:seq s4))) #{ identity partial juxt }))
    
    (is (:play s1))
    (is (:play s2))
    (is (:play s3))
    (is (:play s4))

    (is (not (nil? (ex-data s5))))
    )
  )

(deftest play-sequence-with-chords-test
  (let [test-set (-> (ps/new-sequence)
                     (ps/add-to-sequence (fn [t] {:l "A" :dt t :ct (System/currentTimeMillis)}) 500)
                     (ps/add-to-sequence (fn [t] {:l "B" :dt t :ct (System/currentTimeMillis)}) 1000)
                     (ps/add-to-sequence (fn [t] {:l "C" :dt t :ct (System/currentTimeMillis)}) 1000)
                     (ps/add-to-sequence (fn [t] {:l "D" :dt t :ct (System/currentTimeMillis)}) 1000)
                     (ps/add-to-sequence (fn [t] {:l "E" :dt t :ct (System/currentTimeMillis)}) 2000)
                     (ps/add-to-sequence (fn [t] {:l "F" :dt t :ct (System/currentTimeMillis)}) 2000)
                     (ps/add-to-sequence (fn [t] {:l "G" :dt t :ct (System/currentTimeMillis)}) 2600)
                     (ps/play)
                     ;; Now if we wanted this to be a production app we would add a watcher looking
                     ;; for the play status to flip to false or seq to empty but instead do this
                     ( (fn [a] (Thread/sleep 3500 ) @a)) )
        diffed-set   (map (fn [p n] (reduce
                                     (fn [s k]
                                       (assoc s (keyword (str (name k) "_diff"))  (- (k n) (k p)))) p [ :dt :ct ]))
                          (:return-values test-set)
                          (rest (:return-values test-set)))

        close   (fn [a b] (< (max (- a b) (- b a)) 2))
        ]
    (is (every? identity (map #( close (:dt_diff %) (:ct_diff %)) diffed-set)))
    (is (= (count (:return-values test-set)) 7))
    (is (= (set (map :l (:return-values test-set))) #{ "A" "B" "C" "D" "E" "F" "G" } ) )
    (is (every? identity (map #( close (:dt %) %2 ) (:return-values test-set) (list 500 1000 1000 1000 2000 2000 2600))))
    )
  )


;; (run-tests)


