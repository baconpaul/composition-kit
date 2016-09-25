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

(phrase
 (pitches :a4 :b4 :c4 :a4)
 (durations 1 1 2 4)
 (dynamics 70 80 127 90))

