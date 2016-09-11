(ns composition-kit.example.raw-midi
  (:require [composition-kit.midi-util :as mid])
  (:require [composition-kit.physical-sequence :as psq]))

;; This example shows the raw midi APIs. Probably you don't want to use antyhing at this level in composition but it's handy
;; to have an example kicking around to make sure it all still works.

;; The basic model is we have a sequence onto which we can push events (which are functions at a relative time in miliseconds).
;; Those events will then get played (or stopped) at a later date when you call play on them. All agents and stuff in the background.
;; The functions you want to schedule are ones which send midi events to a connected midi reciever. So lets do a little example of
;; playing a single note.

(let [rcv (mid/get-opened-receiver)]
  (-> (msq/new-sequence)
      (msq/add-to-sequence (mid/send-note-on rcv 1 60 100) 0)
      (msq/add-to-sequence (mid/send-note-off rcv 1 60) 2000)
      (msq/play)))

;; Note the alternate syntax where add-to-sequence can take a set of function time pairs. Here's a lovely tritone
(let [rcv (mid/get-opened-receiver)]
  (-> (msq/new-sequence)
      (msq/add-to-sequence
       (mid/send-note-on rcv 1 60 100) 0
       (mid/send-note-on rcv 1 63 100) 1000
       (mid/send-note-off rcv 1 60) 2000
       (mid/send-note-on rcv 1 66 80) 2000
       (mid/send-note-off rcv 1 63) 3000
       (mid/send-note-off rcv 1 66) 3000
       )
      (msq/play)))

;; You don't have to add the events in order. They sort themselves out behind the scene. Here's a way to generate an
;; increasingly legato whole tone scale, for instance, by mapping across ranges. Now this is gross because the state
;; object is silently collecting state inside itself and breaks the immutability contract through this API.
(let [rcv (mid/get-opened-receiver)]
  (-> (msq/new-sequence)
      (#(reduce (fn [s d]
                  (msq/add-to-sequence s
                                       (mid/send-note-on rcv 1 (+ (* 2 d) 60) 100)
                                       (* 250 d)))
                % (range 8)))
      (#(reduce (fn [s d]
                  (msq/add-to-sequence s
                                       (mid/send-note-off rcv 1 (+ (* 2 d) 60))
                                       (+(* 300 d) 250)))
                % (range 8)))
      (msq/play)))


;; You don't just have to send notes. You can also send control changes and any other midi event (like pitch bends or what not).
;; You need to make sure your synth on the other end can recieve them though. With logic this took me a few seconds to figure out.
;; Here's what I did
;; + Put a synth on a track. Configure it in alchemy or what not
;; + highlight the track and press 'b' to see the performance controls
;; + click on the knob you want to automate
;; + click 'learn'
;; + send one control message. For instance
;;     ((mid/send-control-change (mid/get-opened-receiver) 1 -cc- 64) 0)
;; + You should see that learned

;; And then you can do something like this
(let [rcv (mid/get-opened-receiver)]
  (-> (msq/new-sequence)
      (#(reduce (fn [s v]
                  (msq/add-to-sequence
                   s
                   (mid/send-note-on rcv 1 (+ 60 ( * 3 v)) 100)  (* 250 v)
                   (mid/send-note-off rcv 1 (+ 60 ( * 3 v)))  (+ (* 400 v) 400)))
                % (range 10)))
      (#(reduce (fn [s v]
                  (msq/add-to-sequence
                   s
                   (mid/send-control-change rcv 1 52 (mod v 127)) v))
                % (range 1000)))
      (msq/play))
  1)




