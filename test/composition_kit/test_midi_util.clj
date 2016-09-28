(ns composition-kit.test-midi-util
  (use clojure.test)
  (require [composition-kit.midi-util :as m])
  )

(deftest midi-messages
  (let [non (m/note-on 12 23 34)
        nof (m/note-off 11 22)
        cc  (m/control-change 10 12 14)
        pb  (m/pitch-bend 9 13 27)]
    (is (= (.getChannel non) 12))
    (is (= (.getData1 non) 23))
    (is (= (.getData2 non) 34))
    (is (= (.getCommand non) (javax.sound.midi.ShortMessage/NOTE_ON)))

    (is (= (m/message-to-map non) { :channel 12 :data1 23 :data2 34 :command javax.sound.midi.ShortMessage/NOTE_ON } ))

    (is (= (.getChannel nof) 11))
    (is (= (.getData1 nof) 22))
    (is (= (.getData2 nof) 0))
    (is (= (.getCommand nof) (javax.sound.midi.ShortMessage/NOTE_OFF)))

    (is (= (.getChannel cc) 10))
    (is (= (.getData1 cc) 12))
    (is (= (.getData2 cc) 14))
    (is (= (.getCommand cc) (javax.sound.midi.ShortMessage/CONTROL_CHANGE)))

    (is (= (.getChannel pb) 9))
    (is (= (.getData1 pb) 13))
    (is (= (.getData2 pb) 27))
    (is (= (.getCommand pb) (javax.sound.midi.ShortMessage/PITCH_BEND)))

    )
  )

;; Testing send message is really hard unless I also implement recieve message and listen on IAC. Do that later
;;(let [r (midi-util/get-OpenedTransmitter)
;;q (midi-util/get-OpenedReceiver)]
;;(midi-util/register-transmitter-callback r (fn [m t] (println "YEEHAH" m t )))
;;( (midi-util/send-control-change q 2 3 4 ) 0 )
;;)

(deftest midi-transport-has-fidelity
  (let [r (m/get-opened-receiver)
        t (m/get-opened-transmitter)
        callback-store (atom [])
        ]
    (is (not (nil? r)))
    (is (not (nil? t)))
    (is (not (nil? (m/register-transmitter-callback
                    t
                    (fn [msg time] (swap! callback-store conj (m/message-to-map msg)))
                    ))))
    (let [test-midi-cycle
          (do
            ((m/send-control-change r 0 12 72) 0)
            (Thread/sleep 1)
            ((m/send-control-change r 1 17 73) 1)
            (Thread/sleep 1)
            ((m/send-note-on r 2 18 120) 2)
            (Thread/sleep 1)
            ((m/send-note-off r 2 18) 2)
            (loop [ct 0]
              (if (or (= (count @callback-store) 4) (== ct 10)) @callback-store
                  (do
                    (Thread/sleep 100)
                    (recur (inc ct))))))]
      (is (= (map :channel test-midi-cycle) '(0 1 2 2)))
      (is (= (map :command test-midi-cycle) (list javax.sound.midi.ShortMessage/CONTROL_CHANGE
                                                  javax.sound.midi.ShortMessage/CONTROL_CHANGE
                                                  javax.sound.midi.ShortMessage/NOTE_ON
                                                  javax.sound.midi.ShortMessage/NOTE_OFF
                                                  )))
      (is (= (map :data1 test-midi-cycle) '(12 17 18 18)))
      (is (= (map :data2 test-midi-cycle) '(72 73 120 0)))
      )
    )
  )

(deftest midi-instrument-api
  (let [m (m/midi-instrument "Bus 1" 0)
        t (m/get-opened-transmitter)
        callback-store (atom [])
        ]
    (is (not (nil? m)))
    (is (not (nil? t)))
    (is (not (nil? (m/register-transmitter-callback
                    t
                    (fn [msg time] (swap! callback-store conj (m/message-to-map msg)))
                    ))))
    (let [test-midi-cycle
          (do
            ((m/send-control-change (:receiver m) (:channel m) 12 72) 0)
            (Thread/sleep 1)
            (loop [ct 0]
              (if (or (= (count @callback-store) 1) (== ct 10)) @callback-store
                  (do
                    (Thread/sleep 100)
                    (recur (inc ct))))))]
      (is (= (map :channel test-midi-cycle) '(0)))
      (is (= (map :command test-midi-cycle) (list javax.sound.midi.ShortMessage/CONTROL_CHANGE)))

      (is (= (map :data1 test-midi-cycle) '(12)))
      (is (= (map :data2 test-midi-cycle) '(72)))

      (is (= (m/midi-instrument "Bus 1" 13) (m/midi-instrument 13)))
      )
    )
  )

;;mid
;; (run-tests)

