(ns composition-kit.compositions.folk-dances.folk-dances-2
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))


(def lead-synth (midi/midi-instrument 0))
(def piano (midi/midi-instrument 1))

(def clock (tempo/constant-tempo 5 4 105))

(def lead
  (let [brk       true
        
        melody-structure
        [[:ees4 2 70] [:d4 3 65 brk]
         [:ees4 1 72] [:f4 1 74] [:g4 3 77 brk]
         
         [:aes4 2 80] [:g4 3 76 brk]
         [:f4 1 71] [:g4 1 76] [:ees4 1 71] [:d4 1 67] [:c4 1 64] [:d4 1 65] [:bes3 4 58]

         [:ees4 2 70] [ :f4 3 74 brk]
         [:g4 1 75] [:aes4 1 78] [:bes4 3 82 brk]

         [:aes4 2 80] [:g4 3 76 brk]
         [:f4 1 71 ] [:g4 1 76] [:ees4 1 71] [:d4 1 67] [:c4 1 64] [:d4 1 65] [:bes3 4 58 brk]

         [:bes3 2 65] [:ees4 3 60 brk]
         [:d4 2 65] [:ees4 3 60 brk]
         [:bes3 2 65] [:ees4 3 60 brk]
         [:d4 2 65] [:aes4 2 72] [:g4 1 70] [:f4 1 66] [:g4 1 68] [:ees4 2 62]
         [:d4 1 70] [:ees4 1 70] [:c4 1 68] [:d4 1 72] [:bes3 1 68 brk]

         [:bes4 2 65] [:ees5 3 60 brk]
         [:d5 2 65] [:ees5 3 60 brk]
         [:bes4 2 65] [:ees5 3 60 brk]
         [:d5 2 65] [:aes5 2 72] [:g5 1 70] [:f5 1 66] [:g5 1 68] [:ees5 2 62]
         [:d5 1 70] [:ees5 1 70] [:c5 1 68] [:d5 1 72] [:bes4 1 68 brk]

         ]

        a-to-n
        (fn [sp [pitch dur vel bk]]
          (-> sp
              (update-in [:end] + dur)
              (update-in
               [:notes]
               concat [(i/notes-with-duration pitch dur (:end sp) (if (nil? bk) (* 1.01 dur) (* 0.94 dur)))]))
          )
        ;;          (i/notes-with-duration

        music
        (-*>
         (raw-sequence (:notes (reduce a-to-n {:end 0 :notes []} melody-structure)))
         (ls/explicit-segment-dynamics (map #(nth % 2) melody-structure))
         (ls/transpose 12)
         )
        ]
    (>>>
     (rest-for 10)
     music)
    )
  )

(defn try-out [p i]
  (-> p (on-instrument i) (with-clock clock) (midi-play)))

(def piano-m
  (let [lh-one (<*>
                (>>>
                 (-*>
                  (lily "<ees ees'>4 <a' bes ees> <aes bes d> <ees' aes a> <d g aes>" :relative :c2)
                  (ls/line-segment-dynamics 0 77 1 65 2 61 4 73))
                 (-*>
                  (lily "<ees ees'>4 <a' bes ees> <aes bes d> <ees' aes a>2" :relative :c2)
                  (ls/line-segment-dynamics 0 77 1 65 2 61 4 73))
                 )
                (pedal-held-and-cleared-at 0 4.9 5 9.9))

        lh-two (<*>
                (>>>
                 (-*>
                  (lily "<ees ees'>1 r4" :relative :c2)
                  (ls/explicit-segment-dynamics '(78)))
                 (-*>
                  (lily "<ees ees'>4 <a' bes ees> <aes bes d> <a bes ees>2" :relative :c2)
                  (ls/line-segment-dynamics 0 77 1 65 2 61 4 73)))
                (pedal-held-and-cleared-at 0 9.9))

        rh-two (<*>
                (-*>
                 (lily "  <bes a g>2 <bes a ges>2. <bes' a g>4 <bes a e>4 <bes a d,>2." :relative :c4)
                 (ls/explicit-segment-dynamics '(79 74 81 77 72))))
                                        ;_ (loop-n (try-out rh-two piano) 2)
        ]
    (<*>
     (>>>
      lh-one
      lh-two
      )
     (>>>
      (rest-for 10)
      rh-two
      )
     )
    )
  )

(def final-song
  (<*>
   (-> lead (on-instrument lead-synth))
   ;;(-> piano-m (on-instrument piano))
   )
  )

(def play-it true)
(def player
  (when play-it
    (->
     final-song
     (with-clock clock)
     (midi-play
      :beat-zero -1
      ;;:beat-end 0

      ))))

;;(def x (composition-kit.events.physical-sequence/stop player))

