(ns composition-kit.midi-util
  (:import (javax.sound.midi MidiSystem ShortMessage))
  )

(defn ^{:private true} getOpenedReceiver-unmemo
  ([]  (getOpenedReceiver-unmemo "Bus 1"))
  ([name]
   (->> (MidiSystem/getMidiDeviceInfo)
        (filter #(= (.getName %) name))
        (map #(MidiSystem/getMidiDevice %))
        (filter #(>= (.getMaxTransmitters %) 0))
        first
        (#(do (.open %) %))
        (#(.getReceiver %))
        )
   )
  )

(def getOpenedReceiver (memoize getOpenedReceiver-unmemo))

;; Wrappers for short message types
(defn ^:private gen-short-message-func
  ([msg] (gen-short-message-func msg 3))
  ([msg args]
   (case args
     2   (fn [a b] (ShortMessage. msg a b 0))
     3   (fn [a b c] (ShortMessage. msg a b c))))) 

(def note-on (gen-short-message-func ShortMessage/NOTE_ON))
(def note-off (gen-short-message-func ShortMessage/NOTE_OFF 2))
(def control-change (gen-short-message-func ShortMessage/CONTROL_CHANGE))
(def pitch-bend (gen-short-message-func ShortMessage/PITCH_BEND))


(defn gen-send [rcv msg] (fn [time] (.send rcv msg -1)))

(defn ^:private gen-send-message-func[msg]
  (fn gen-send-internal
    ([rcv a b]   (gen-send-internal rcv a b 0))
    ([rcv a b c] (let [msg (ShortMessage. msg a b c)]
                   (fn [time] (.send rcv msg -1))))))


(def send-note-on (gen-send-message-func ShortMessage/NOTE_ON))
(def send-note-off (gen-send-message-func ShortMessage/NOTE_OFF))
(def send-control-change (gen-send-message-func ShortMessage/CONTROL_CHANGE))
(def send-pitch-bend (gen-send-message-func ShortMessage/PITCH_BEND))

