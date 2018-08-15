(ns composition-kit.scratch
  (require [composition-kit.music-lib.midi-util]
           [composition-kit.events.transport-window]))
;; Scratch.clj contains little scratch test cases I use to make sure things work

;; Test timecode transport window
(defn make-tc-transport-window []
  (let [transport (composition-kit.events.transport-window/make-transport-window "MTC")]
    (fn [action data]
      (condp = action
        :midi-clock-position
        nil

        :smtpe-timecode-time
        ((:assoc transport) :time data)

        (println "unhandled - " action data)
        )
      )
    )
  )


(def iac2 (composition-kit.music-lib.midi-util/get-opened-transmitter "Bus 2"))

;;(def msgatom (atom []))
;;(register-transmitter-callback iac2 (fn [m t] (swap! msgatom conj m)))


(composition-kit.music-lib.midi-util/register-transmitter-callback
 iac2
 (composition-kit.music-lib.midi-util/make-time-code-interpret (make-tc-transport-window)))

#_(let [m (make-time-code-interpret)]
    (doall (map #(m % 0) @msgatom))
    )
