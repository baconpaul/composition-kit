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
        ]
    (is (not (nil? r)))
    (is (not (nil? t)))
    )
  )


;;mid(run-tests)
