(ns composition-kit.music-lib.midi-util
  (:import (javax.sound.midi MidiSystem ShortMessage MidiDevice MidiDevice$Info Transmitter Receiver))
  (require [composition-kit.music-lib.logical-sequence :as ls]
           [composition-kit.events.transport-window :as tw])

  )

;; A set of functions to allow me to interact with javax.sound.midi in a slightly less painful way from clojure.


;; I keep a running global of the pipes I've opened so I can do an all-pipes-shutdown.
(def opened-recv (atom []))

(defn ^{:private true} get-opened-receiver-unmemo
  ([]  (get-opened-receiver-unmemo "Bus 1"))
  ([name]
   (let [device-info       (MidiSystem/getMidiDeviceInfo) 
         named-device-info (filter #(= (.getName ^MidiDevice$Info %) name) device-info)
         devices           (map #(MidiSystem/getMidiDevice ^MidDevice$Info %) named-device-info)
         receivables       (filter #(>= (.getMaxTransmitters ^MidiDevice %) 0) devices)
         _                 (when (empty? receivables)
                             (throw (ex-info "Unable to resolve midi device name"
                                             {:name name
                                              :devices (map #(.getName ^MidiDevice$Info %) device-info)
                                              })))
         receivable        (first receivables)
         result            (do 
                             (.open ^MidiDevice receivable)
                             (.getReceiver ^MidiDevice receivable))]
     (swap! opened-recv conj name)
     result)
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
(defn midi-port
  ([channel] (midi-port "Bus 1" channel))
  ([bus channel]
   {
    :receiver (get-opened-receiver bus)
    :channel  channel
    })
  )


;; Now make the instrument maps. Make an abstraction name to create it for now
(defn midi-instrument-map [] {})

(defn midi-instrument-from-name-and-port [name port] {:name name :port port})
(defn add-midi-instrument [m name port]
  (assoc m name (midi-instrument-from-name-and-port name port))
  )

(defn all-notes-off
  ([] (doseq [r @opened-recv] (all-notes-off r)))
  ([b]
   (let [r (get-opened-receiver b)
         ]
     (doall
      (map (fn [chan]
             ((send-control-change r chan 64 0) 0);; no pedal
             (doall (map #(do
                            (when (zero? (mod % 20)) (java.lang.Thread/sleep 1))
                            ((send-note-off r chan %) 0)) (range 128))))
           (range 16))))
   true
   )
  )


(defn pair-as-14-bits [b1 b2]
  ;;unsigned short CombineBytes(unsigned char First, unsigned char Second)
  ;; unsigned short _14bit                ;
  ;;_14bit = (unsigned short)Second      ;
  ;;_14bit <<= 7                         ;
  ;;_14bit |= (unsigned short)First      ;
  ;;return(_14bit)                       ;
  (-> b2
      (bit-and 0xFF)
      (bit-shift-left 7)
      (bit-or (bit-and 0xFF b1))
      )
  )

(defn make-time-code-interpret []
  (let [transport (tw/make-transport-window "MTC")
        posn (atom {:sixteenths 0 :frames 0})
        tc-byte-parse (fn [ti]
                        (let [t (bit-and 0xFF ti)
                              high-n (-> t
                                         (bit-and 0xF0)
                                         (bit-shift-right 4))
                              low-n (bit-and t 0x0F)
                              ]
                          {:high high-n :low low-n}
                          )
                        )

        time-code (atom {:frames 0 :seconds 0 :minutes 0 :hours 0 :fps 24})
        ]
    (fn [m t]
      (let [d (.getMessage m)
            type (bit-and 0xFF (nth d 0))
            ]
        (condp = type
          javax.sound.midi.ShortMessage/SONG_POSITION_POINTER
          (let [posn-in-16ths (pair-as-14-bits (nth d 1) (nth d 2))]
            (swap! posn assoc :sixteenths posn-in-16ths)
            ((:assoc transport) :midi-clock-position @posn)
            )
          
          javax.sound.midi.ShortMessage/MIDI_TIME_CODE
          ;; https://www.recordingblogs.com/wiki/midi-quarter-frame-message
          (let [tc (tc-byte-parse (nth d 1))
                l (:low tc)]
            (condp = (:high tc)
              0 ; frames low
              (swap! time-code assoc :frames l )
              1 ; frames high
              (swap! time-code update-in [:frames] bit-or (bit-shift-left l 4))
              2 ; seconds low
              (swap! time-code assoc :seconds l )
              3 ; seconds high
              (swap! time-code update-in [:seconds] bit-or (bit-shift-left l 4))
              4 ; minutes low
              (swap! time-code assoc :minutes l )
              5 ; minutes high
              (swap! time-code update-in [:minutes] bit-or (bit-shift-left l 4))
              6 ; hours low
              (swap! time-code assoc :hours l )
              7 ; 0 rr h where rr is frame rate 00->24 01->25 10->29.97 11->30
              (let [hb (bit-and l 0x1)
                    rr (bit-shift-right (bit-and l 0x6) 1)
                    ]
                (swap! time-code update-in [:hours] bit-or (bit-shift-left hb 4))
                (swap! time-code assoc :fps (condp = rr 0 24 1 25 2 29.97 3 30))
                
                (let [full-time (+
                                 (* (- (:hours @time-code) 1) 60 60)
                                 (* (:minutes @time-code) 60)
                                 (:seconds @time-code)
                                 (/ (:frames @time-code) 1.0 (:fps @time-code))
                                 
                                 )]
                  ((:assoc transport) :time (* 1000 full-time))
                  )
                ) 
              )

            )


          javax.sound.midi.ShortMessage/TIMING_CLOCK
          (do 
            (if (= (:frames @posn) 5)
              (do (swap! posn (fn [pm] {:sixteenths (inc (:sixteenths pm)) :frames 0}) ))
              (do (swap! posn update-in [:frames] inc))
              )
            ((:assoc transport) :midi-clock-position @posn)
            )

          javax.sound.midi.ShortMessage/START
          nil; (println "START" )

          javax.sound.midi.ShortMessage/STOP 
          nil; (println "STOP" )

          javax.sound.midi.ShortMessage/CONTINUE
          nil; (println "CONTINUE " (.getLength m))

          (println "Unknown timecode message " type)
          )
        )
      )
    )
  )

(def iac2 (get-opened-transmitter "Bus 2"))

;;(def msgatom (atom []))
;;(register-transmitter-callback iac2 (fn [m t] (swap! msgatom conj m)))


(register-transmitter-callback  iac2 (make-time-code-interpret))

_(let [m (make-time-code-interpret)]
   (doall (map #(m % 0) @msgatom))
   )







