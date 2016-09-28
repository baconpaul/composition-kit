(ns composition-kit.midi-util
  (:import (javax.sound.midi MidiSystem ShortMessage MidiDevice MidiDevice$Info Transmitter Receiver))
  (require [composition-kit.logical-sequence :as ls])
  ;;(:require [tupelo.core     :as t])
  )
;;(t/refer-tupelo)

(defn ^{:private true} get-opened-receiver-unmemo
  ([]  (get-opened-receiver-unmemo "Bus 1"))
  ([name]
   (as-> (MidiSystem/getMidiDeviceInfo) it
     (filter #(= (.getName ^MidiDevice$Info %) name) it)
     (map #(MidiSystem/getMidiDevice ^MidDevice$Info %) it)
     (filter #(>= (.getMaxTransmitters ^MidiDevice %) 0) it)
     (if (empty? it) (throw (ex-info "No midi devices with recievers" {:name name})) it)
     (first it)
     (do (.open ^MidiDevice it) it)
     (.getReceiver ^MidiDevice it)
     )
   )
  )

(defn ^{:private true} get-opened-transmitter-unmemo
  ([]  (get-opened-transmitter-unmemo "Bus 1"))
  ([name]
   (as-> (MidiSystem/getMidiDeviceInfo) it
     (filter #(= (.getName ^MidiDevice$Info %) name) it)
     (map #(MidiSystem/getMidiDevice ^MidiDevice$Info %) it)
     (filter #(>= (.getMaxReceivers ^MidiDevice %) 0) it)
     (first it)
     (do (.open ^MidiDevice it) it)
     (.getTransmitter ^MidiDevice it)
     )
   )
  )


(def get-opened-receiver (memoize get-opened-receiver-unmemo))
(def get-opened-transmitter (memoize get-opened-transmitter-unmemo))

;; Wrappers for short message types
(defn ^:private gen-short-message-func
  ([msg] (gen-short-message-func msg 3))
  ([msg args]
   (case (int args)
     2   (fn [a b] (ShortMessage. msg a b 0))
     3   (fn [a b c] (ShortMessage. msg a b c))))) 

(def note-on (gen-short-message-func ShortMessage/NOTE_ON))
(def note-off (gen-short-message-func ShortMessage/NOTE_OFF 2))
(def control-change (gen-short-message-func ShortMessage/CONTROL_CHANGE))
(def pitch-bend (gen-short-message-func ShortMessage/PITCH_BEND))


(defn gen-send [^Receiver rcv msg] (fn [time] (.send rcv msg -1)))

(defn ^:private gen-send-message-func[msg]
  (fn gen-send-internal
    ([^Receiver rcv a b]   (gen-send-internal rcv a b 0))
    ([^Receiver rcv a b c] (let [msg (ShortMessage. msg a b c)]
                             (fn [time] (.send rcv msg -1))))))


(def send-note-on (gen-send-message-func ShortMessage/NOTE_ON))
(def send-note-off (gen-send-message-func ShortMessage/NOTE_OFF))
(def send-control-change (gen-send-message-func ShortMessage/CONTROL_CHANGE))
(def send-pitch-bend (gen-send-message-func ShortMessage/PITCH_BEND))

(defn message-to-map [^ShortMessage m]
  {:channel (.getChannel m)
   :command (.getCommand m)
   :data1   (.getData1 m)
   :data2   (.getData2 m)})

;; The transmitter API is a bit clunky if you want raw messages so lets abstract it away a little bit
(defn register-transmitter-callback [ ^Transmitter t f ]
  (.setReceiver t
                (reify javax.sound.midi.Receiver
                  (send [this msg time] (f msg time))))
  t
  )

;; Make a little abstraction for a midi instrument which we can use to pass around state later on.
(defn midi-instrument
  ([channel] (midi-instrument "Bus 1" channel))
  ([bus channel]
   {
    :instrument-type ::midi-instrument
    :receiver (get-opened-receiver bus)
    :channel  channel
    })
  )



