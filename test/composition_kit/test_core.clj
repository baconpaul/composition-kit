(ns composition-kit.test-core
  (use clojure.test)
  (use composition-kit.core)
  (:require [composition-kit.logical-sequence :as ls])
  )

(deftest lily-macro
  (let  [l1 (:composition-payload (lily :relative :c4 "c4 d e"))
         l2 (:composition-payload (lily :relative :c4 "c4 d e r2 c c'"))]
    (is (= (count l1) 3))
    (is (= (count l2) 5)) ;; we can drop the rest
    (is (= (mapcat (comp :notes ls/item-payload) l2) [ :c4 :d4 :e4 :c4 :c5 ] ))
    (is (= (map ls/item-beat l2) [ 0 1 2 5 7 ] ))
    (is (= (:composition-type (lily "c2")) :composition-kit.core/sequence))
    )
  )

(deftest concatenate-operator
  (let [phrase (concatenate (lily "c2 d") (lily "e4 f g e"))]
    (is (= (:composition-type phrase) :composition-kit.core/sequence))
    (is (= (count (:composition-payload phrase)) 6))
    (is (= (map (comp :notes ls/item-payload) (:composition-payload phrase) [ :c4 :d4 :e4 :f4 :g4 :e4])))
    (is (= (map ls/item-beat (:composition-payload phrase)) '(0 2 4 5 6 7)))
    )
  )

(deftest overlay-operator
  (let [phrase (overlay (lily "c2 d") (lily "e4 f g e"))]
    (is (= (:composition-type phrase) :composition-kit.core/sequence))
    (is (= (count (:composition-payload phrase)) 6))
    (is (= (map (comp :notes ls/item-payload) (:composition-payload phrase) [ :c4 :d4 :e4 :f4 :g4 :e4])))
    (is (= (map ls/item-beat (:composition-payload phrase)) '(0 0 1 2 2 3)))
    )
  )

(deftest phrase-operator
  ;; This test isn't awesome. It should really look at contents; but other tests make sure the
  ;; underlying operators work so really test the counts and the exceptions
  (let [p1   (phrase (lily "c2 d"))
        p2   (phrase
              (pitches :a4 :b4 :c4 :a4)
              (durations 1 1 2 4)
              (dynamics 70 80 127 90))
        p3   (phrase
              (lily "c2 d e f")
              (dynamics-at 0 -> 127 4 -> 60 8 -> 100))
        ]
    (is (= (:composition-type p1) (:composition-type p2) :composition-kit.core/sequence))
    (is (= (count (:composition-payload p1)) 2))
    (is (= (count (:composition-payload p2)) 4))
    (is (= (:notes (ls/item-payload (first (:composition-payload p2)))) :a4))
    (is (= (count (:composition-payload p3)) 4))
    )
  ;; Test the throw case
  (is (thrown? Exception (phrase (pitches :a4) (pitches :b4))))
  (is (thrown? Exception (phrase (pitches :a4) (durations 1) (durations 2))))
  (is (thrown? Exception (phrase (pitches :a4) (durations 1) (dynamics 2) (dynamics 20))))
  )

(deftest error-cases
  ;; this one is a macro exception so shows as a compile error
  (is (clojure.string/includes? (try (macroexpand `(dynamics-at 0 1 -> 2))
                                     (catch clojure.lang.ExceptionInfo e (str e)))
                                "Incorrect syntax"))

  ;; you can only overlay sequences so need the intermediat "phrase" operator
  (is (thrown? clojure.lang.ExceptionInfo
               (overlay (lily "c2 d") (dynamics 1 2))))
  (is (thrown? clojure.lang.ExceptionInfo
               (concatenate (lily "c2 d") (dynamics 1 2))))
  )


