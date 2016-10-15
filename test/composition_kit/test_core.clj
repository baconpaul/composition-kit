(ns composition-kit.test-core
  (use clojure.test)
  (use composition-kit.core)
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  )

(map (comp :notes ls/item-payload) (:composition-payload (lily :relative :c4 "c4 d e r2 c c'")))

(deftest parse-macros
  (let  [l1 (:composition-payload (lily :relative :c4 "c4 d e"))
         l2 (:composition-payload (lily :relative :c4 "c4 d e r2 c c'"))
         l3 (:composition-payload (lily :relative :c2 "c4 d e r2 c c'"))
         l4 (:composition-payload (lily :relative :c4 "c4 d <e g> r2 c c'"))
         ]
    (is (= (count l1) 3))
    (is (= (count l2) 6)) 
    (is (= (map (comp :notes ls/item-payload) l2) [ :c4 :d4 :e4 nil :c4 :c5 ] ))
    (is (= (map (comp :notes ls/item-payload) l3) [ :c2 :d2 :e2 nil :c2 :c3 ] ))
    (is (= (map (comp :notes ls/item-payload) l4) [ :c4 :d4 [ :e4 :g4 ] nil :c4 :c5 ] ))
    (is (= (map ls/item-beat l2) [ 0 1 2 3 5 7 ] ))
    (is (= (:composition-type (lily "c2")) :composition-kit.core/sequence))
    )

  (let [ss (step-strings
            :c2  "X...X...X.X.X..."
            :d2  ".A....A.......A."
            )
        ss2 (step-strings
             [:c2  "X...X...X.X.X..."
              :d2  ".A....A.......A."]
             )
        ]
    (is (= (count (:composition-payload ss)) 32))
    (is (= (count (filter #(= (ls/item-type %) :composition-kit.music-lib.logical-sequence/notes-with-duration) (:composition-payload ss))) 8))
    (is (= (count (:composition-payload ss2)) 32))
    (is (= (count (filter #(= (ls/item-type %) :composition-kit.music-lib.logical-sequence/notes-with-duration) (:composition-payload ss2))) 8))
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

(deftest loop-n-operator
  (let [phrase (phrase (lily "c2 d"))
        double (loop-n phrase 2)
        quad   (loop-n phrase 4)
        alt-quad (loop-n double 2)
        ccp    #(count (:composition-payload %))]
    ;; This can be a light test since the udnerlying loop operator is well tested elsewhere
    (is (= (ccp double) 4))
    (is (= (ccp quad) (ccp alt-quad) 8))
    )
  )

(deftest pedal-operator
  ;; a rudimentrary test but makes sure it goes up and down right number of times at least
  (let [pedal (pedal-held-and-cleared-at 0 2 4)
        levs  (map (comp :value ls/item-payload) (:composition-payload pedal))
        zts   #(or (= % 0) (= % 127))
        check (->> (:composition-payload pedal)
                   (map (comp :value ls/item-payload))
                   (partition-by zts)
                   (filter (comp zts first))
                   (map distinct)
                   flatten)
        ]
    (is (= check '(0 127 0 127 0)))
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

(deftest other-operators
  ;;(do
  (let [p1 (phrase (lily "c2 d"))
        inst   (midi/midi-instrument 0)
        clock  (tempo/constant-tempo 2 4 200)

        pin (on-instrument p1 inst)
        pcl (with-clock p1 clock)
        pf  (-> p1 (on-instrument inst) (with-clock clock))

        fnt  #(first (:composition-payload %))

        phr  (ls/repeated-note :c4 1/4 16)
        rph  (raw-sequence phr)
        ]
    (is (= inst (ls/item-instrument (fnt pin)) (ls/item-instrument (fnt pf))))
    (is (= clock (ls/item-clock (fnt pcl)) (ls/item-clock (fnt pf))))
    (is (nil? (ls/item-clock (fnt pin))))
    (is (nil? (ls/item-instrument (fnt pcl))))
    (is (= (:composition-payload rph) phr))
    )
  )



(deftest test-error-cases
  ;; this one is a macro exception so shows as a compile error
  (is (clojure.string/includes? (try (macroexpand `(dynamics-at 0 1 -> 2))
                                     (catch clojure.lang.ExceptionInfo e (str e)))
                                "Incorrect syntax"))

  ;; you can only overlay sequences so need the intermediat "phrase" operator
  (is (thrown? clojure.lang.ExceptionInfo
               (overlay (lily "c2 d") (dynamics 1 2))))
  (is (thrown? clojure.lang.ExceptionInfo
               (concatenate (lily "c2 d") (dynamics 1 2))))
  (is (thrown? clojure.lang.ExceptionInfo (midi-play (dynamics 1 2))))

  (is (thrown? clojure.lang.ExceptionInfo  (step-strings :c2 "x" :d2 2)))
  (is (thrown? clojure.lang.ExceptionInfo  (step-strings :c2 "x" "d" "x")))
  (is (thrown? clojure.lang.ExceptionInfo  (step-strings :c2 "x" :d2)))
  )


(deftest test-midi-play
  ;; Again a small test of the wrappers which are more extensively tested elsewhere
  (let [ph     (phrase (lily "c4 d fis8 ees e4"))
        inst   (midi/midi-instrument 0)
        clock  (tempo/constant-tempo 2 4 200)
        plb    (-> ph (on-instrument inst) (with-clock clock))
        callback-store (atom [])
        t (midi/get-opened-transmitter)
        
        _ (midi/register-transmitter-callback
           t
           (fn [msg time] ;; that time is wierd and useless miditime which I didn't hack in so
             (swap! callback-store conj (assoc (midi/message-to-map msg) :time (System/currentTimeMillis))))
           )
        play-agent (midi-play plb)
        expected-length 10
        test-midi-notes-sent
        (do
          (Thread/sleep 1)
          (loop [ct 0]
            (if (or (= (count @callback-store) expected-length) (== ct 10)) @callback-store
                (do
                  (Thread/sleep 200)
                  (recur (inc ct))))))]
    (is (= (count test-midi-notes-sent) expected-length))
    (is (nil? (agent-error play-agent)))

    )

  )

