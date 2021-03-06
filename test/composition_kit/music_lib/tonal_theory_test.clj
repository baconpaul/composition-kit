(ns composition-kit.music-lib.tonal-theory-test
  (use clojure.test)
  (require [composition-kit.music-lib.tonal-theory :as th]))

(deftest notes
  (is (= (:note (th/note-by-name :c4)) :c4) "Identity of notes")

  (is (= (- (:midinote (th/note-by-name :c5)) (:midinote (th/note-by-name :c4))) 12) "Octaves are octaves")

  (is (= (th/name-to-midinote :c4) 60))
  
  (is (every? (map (fn [n] (= (every? #(= (:midinote %) n)) (th/notes-by-midinote n)))) (map (partial + 30) (range 40))) "Enharmonic 1")
  (is (= (:midinote (th/note-by-name :cis3) (th/note-by-name :des3))) "Enharmonic 2")
  )

(deftest intervals
  (is (zero? (th/interval-from-c :c)) "C is c")
  (is (reduce
       #(and %1 %2)
       (flatten
        (for [ mn (map (partial + 60) (range 11)) ]
          (let [ test-notes (th/notes-by-midinote mn) ]
            (for [ test-note test-notes ]
              (is (= (th/interval-from-c (:pitch test-note)) (- mn 60)) (str "Testing interval for " (name (:pitch test-note))))))))))
  (is (= (th/interval-between :a :c) 3))
  (is (= (th/interval-between :c :a) 9))
  (is (= (th/interval-between :cis :a) 8))
  (is (= (th/interval-between :cis :aes) 7))
  (is (= (th/interval-between :dis :aes) 5))
  (is (= (th/interval-between :dis :fees) 0)) 
  )

(deftest scales
  (is (not (nil? (:major (set (th/known-scales))))))
  (is (not (nil? (:dorian (set (th/known-scales))))))
  (is (= (th/scale :major) '(0 2 4 5 7 9 11 12)))
  (is (= (map :pitch (th/scale-to-notes (th/scale :major) (th/note-by-name :c4))) '(:c :d :e :f :g :a :b :c)))
  (is (= (map :pitch (th/scale-to-notes (th/scale :major) (th/note-by-name :dis4))) '(:dis :eis :fiis :gis :ais :bis :ciis :dis)))
  (is (= (map :pitch (th/scale-to-notes (th/scale :major) (th/note-by-name :ees4))) '(:ees :f :g :aes :bes :c :d :ees)))
  (is (= (th/scale-pitches :c4 :major) '(:c4 :d4 :e4 :f4 :g4 :a4 :b4 :c5)))

  )

(deftest various-ops
  (let [bn (th/note-by-name :cis2)
        up3 (th/transpose bn 3)
        ]
    (is (= (:midinote up3) (+ 3 (:midinote bn))))
    (is (th/enharmonic-equal? up3 (th/note-by-name :e2)))))
    

;; (run-tests)

