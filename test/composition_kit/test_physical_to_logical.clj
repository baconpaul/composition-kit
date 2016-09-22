(ns composition-kit.test-physical-to-logical
  (use clojure.test)
  (:require [composition-kit.logical-sequence :as ls])
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.tempo :as tempo])
  (:require [composition-kit.physical-sequence :as ps])
  (:require [composition-kit.tonal-theory :as th])
  (:require [composition-kit.physical-to-logical :as ptol])
  (:import (javax.sound.midi MidiSystem ShortMessage))
  )

(deftest simple-conversion
  (let [mphrase (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 ] [ 1 1/2 1/2 ] )
        controls (ls/concrete-logical-sequence (map (fn [v t] (ls/control-event 64 v t)) [ 127 64 0] [ 0 1 3/2 ] ))
        phrase (ls/merge-sequences mphrase controls)
        inst   (midi/midi-instrument 0)
        bpm    140
        clock  (tempo/constant-tempo 2 4 bpm)
        pseq   (ptol/schedule-logical-on-physical (ps/new-sequence) phrase inst clock)

        t (midi/get-opened-transmitter)
        callback-store (atom [])
        expected-length   (+ (count controls) (* 2 (count mphrase))) ;; noteon noteoff
        ]
    (is (= expected-length 9))
    (is (= (count (:seq pseq)) expected-length)) ;; Note on and note off events
    (is (not (nil? (midi/register-transmitter-callback
                    t
                    (fn [msg time] ;; that time is wierd and useless miditime which I didn't hack in so
                      (swap! callback-store conj (assoc (midi/message-to-map msg) :time (System/currentTimeMillis))))
                    ))))
    
    ;; OK so lets try and see if we see the messages back
    (let [play-agent (ps/play pseq)
          test-midi-notes-sent
          (do
            (Thread/sleep 1)
            (loop [ct 0]
              (if (or (= (count @callback-store) expected-length) (== ct 10)) @callback-store
                  (do
                    (Thread/sleep 200)
                    (recur (inc ct))))))]
      ;; did we get 6 notes and no errors?
      (is (= (count test-midi-notes-sent) expected-length))
      (is (nil? (agent-error play-agent)))

      ;; Was everything on the channel of the midi instrument
      (is (every? #(= (:channel %) (:channel inst)) test-midi-notes-sent))
      ;; Are the notes what we expected
      (let [ons (filter #(= (:command %) ShortMessage/NOTE_ON) test-midi-notes-sent)
            offs (filter #(= (:command %) ShortMessage/NOTE_OFF) test-midi-notes-sent)
            spb  (/ 60 bpm)
            ]
        (is (= (count ons) (count offs) 3))
        (is (= (map :data1 ons) (map th/name-to-midinote [:c4 :d4 :e4])))
        (is (= (map :data1 offs) (map th/name-to-midinote [:c4 :d4 :e4])))
        (is (<= (reduce
                 +
                 (map -
                      (map #(- (:time %) (:time (first ons))) ons)
                      (map (comp int (partial * 1000 )) [ 0 spb (* 1.5 spb)]))
                 ) 2))
        )

      )
    )

  )

(deftest schedule-a-loop
  (let [phrase (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 ] [ 1 1/2 1/2 ] )
        loop   (ls/loop-sequence phrase 10)
        pseq   (ptol/schedule-logical-on-physical
                (ps/new-sequence)
                loop
                (midi/midi-instrument 0)
                (tempo/constant-tempo 4 4 120))]
    (is (= (count (:seq pseq)) 60))
    )
  )

(deftest shorthand-schedule
  (let [phrase (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 ] [ 1 1/2 1/2 ])
        pseq   (ptol/schedule-logical-on-physical
                (ps/new-sequence)
                phrase
                (midi/midi-instrument 0)
                (tempo/constant-tempo 4 4 120))
        shortseq (ptol/create-and-schedule phrase (midi/midi-instrument 0)
                                           (tempo/constant-tempo 4 4 120))]
    (is (= (count (:seq pseq)) (count (:seq shortseq))))
    (is (= (map ls/item-beat (:seq pseq)) (map ls/item-beat (:seq shortseq))))
    )
  )


(deftest schedule-dynamics
  (let [phrase (ls/line-segment-dynamics (ls/loop-sequence (ls/sequence-from-pitches-and-durations [ :c4 :d4 :e4 ] [ 1 1/2 1/2 ] ) 10)
                                         0 10
                                         20 120)
        inst   (midi/midi-instrument 0)
        bpm    200
        clock  (tempo/constant-tempo 2 4 bpm)
        pseq   (ptol/schedule-logical-on-physical (ps/new-sequence) phrase inst clock)

        t (midi/get-opened-transmitter)
        callback-store (atom [])
        ]
    (is (not (nil? (midi/register-transmitter-callback
                    t
                    (fn [msg time] ;; that time is wierd and useless miditime which I didn't hack in so
                      (swap! callback-store conj (assoc (midi/message-to-map msg) :time (System/currentTimeMillis))))
                    ))))
    
    ;; OK so lets try and see if we see the messages back
    (let [play-agent (ps/play pseq)
          test-midi-notes-sent
          (do
            (Thread/sleep 1)
            (loop [ct 0]
              (if (or (= (count @callback-store) 60) (== ct 10)) @callback-store
                  (do
                    (Thread/sleep 1000)
                    (recur (inc ct))))))]
      ;; did we get 6 notes and no errors?
      (is (= (count test-midi-notes-sent) 60))
      (is (nil? (agent-error play-agent)))

      ;; Was everything on the channel of the midi instrument
      (is (every? #(= (:channel %) (:channel inst)) test-midi-notes-sent))
      ;; Are the notes what we expected
      (let [ons (filter #(= (:command %) ShortMessage/NOTE_ON) test-midi-notes-sent)
            offs (filter #(= (:command %) ShortMessage/NOTE_OFF) test-midi-notes-sent)
            volumes (map :data2 ons)
            ]
        (is (= (count ons) (count offs) 30))
        (is (every? identity (map (fn [a b] (< a b)) volumes (rest volumes))))
        (is (= (first volumes) 10))
        )

      )
    )

  )

