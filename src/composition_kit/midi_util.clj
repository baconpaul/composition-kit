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
(defn note-on [chan pitch vel] (ShortMessage. ShortMessage/NOTE_ON chan pitch vel))
(defn note-off [chan pitch] (ShortMessage. ShortMessage/NOTE_OFF chan pitch 0))

