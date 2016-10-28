(ns composition-kit.compositions.folk-dances.folk-dances-3
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

;; Whack together a clock here
(defn near-constant-tempo [per-meas unit bpm]
  (-> (tempo/constant-tempo per-meas unit bpm)
      (assoc :clocktype ::near-constant)))

(derive ::near-constant :composition-kit.music-lib.tempo/constant)
(defmethod tempo/beats-to-time ::near-constant [clock beats]
  (let [b-mod (mod beats 15)
        measure (int (/ beats 15))
        b-line (partition 2  [0 0 4 0.12 7 -0.08 10 0.11 15 0])

        b4 (last (filter #(>= b-mod (first %)) b-line))
        af (first (filter #(< b-mod (first %)) b-line))

        lin-fac (/ (- b-mod (first b4)) (- (first af) (first b4)))

        fac (* (+ (* lin-fac (second b4))
                  (* (- 1 lin-fac) (second af))
                  ) (:spb clock))
        fac (if (and (>= measure 8) (<= measure 10)) 0 fac)
        ]
    (+ (* beats (:spb clock)) fac)))

(def piano (midi/midi-instrument 0))
(def sin-bell (midi/midi-instrument 1))
(def clock (near-constant-tempo 15 4 95))
(def con-clock (tempo/constant-tempo 15 4 95))

(defn try-out [p i]
  (-> p (ls/on-instrument i) (ls/with-clock clock) (midi-play :beat-clock clock)))


(def orig-piano-rh
  "<c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2

  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2

  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2
  
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2
  
  <g' bes ees>4 <g bes g'> <g bes ees> <c ees c'>1
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>2. <c ees g>2
  
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>1
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>2. <c ees g>2

  <g des ees>4 <g des g> <g des ees> <ces ees ces'>1
  <g des ees>4 <g des g> <g des ees> <ces ees ces'>2. <c ees c'>2
  
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>1
  <g bes ees>4 <g bes g'> <g bes ees> <c ees c'>2. <c ees g>2

  <des ees bes'>4 <des ees aes> <des ees> <g, bes ees>1
  <c ees aes>4 <c ees g> c <f, aes des>2. <g aes ees'>2
  
  <aes bes ees>4 <aes bes ees> <aes bes ees> <g aes ees'>1
  <g aes ees'>4 <g aes ees'> <g aes ees'> <aes bes ees>2. <ees' bes'>2
  
  <ees c'>4 <ees c'> <ees c'> <ees aes bes>1 <ees c'>4 <ees c'> <ees c'> <aes des>2. <g, aes ees'>2

  <c, ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2

  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2

  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>2. <c ees g>2
  
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees c'>1
  <c ees bes'>4   <c ees bes'>4   <c ees bes'>4   <c ees aes>1")

(def orig-piano-lh
  "
   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   des4 des des des1 des4 des des des2. des2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2

   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   des4 des des des1 des4 des des des2. des2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2

  des4 des des ees f g bes aes aes aes f, aes des f aes
  
   g4 ees bes c1 des4 ees f g aes bes c2
   des4 des des c1 des4 des des ees2. ees,2
  
   aes,4 aes aes aes1 aes4 aes aes aes2. aes2
   aes4 aes aes aes1 aes4 aes aes aes2. aes2
   des4 des des des1 des4 des des des2. des2
   aes4 aes aes aes1 aes4 aes aes aes1" )



(def orig-piano
  (let [in-measure-dyn      (partition 2 [0 1  4 1.2  5 0.8  11 1.15  12 0.5]) ;; this pattern is bad for measures 9 - 11
        mid-measure-dyn     (partition 2 [0 1 3 0.9 6 1.05 6.9 1.02 9 0.97 15 1.15])
        final-mid-measure   (partition 2 [0 0.95 10 1.2 12 0.8])

        by-measure-dyn      (concat (repeat 8 in-measure-dyn)
                                    (repeat 2 mid-measure-dyn)
                                    (repeat 1 final-mid-measure)
                                    (repeat 4 in-measure-dyn))
        
        measure-overall-dyn [42 45 49 47 63 64 67 64 72 76 79 45 47 52 46]
        xpand-dyn (flatten
                   (map (fn [id md inm]
                          (let [ofs (* id 15)]
                            (map (fn [i] [ (+ ofs (first i)) (* (second i) md)]) inm)))
                        (range (count measure-overall-dyn)) measure-overall-dyn by-measure-dyn))

        rh-orig
        (-> (lily orig-piano-rh :relative :c3)
            (ls/hold-for-pct 0.99)
            ((fn [s] (apply ls/line-segment-dynamics (concat [s] xpand-dyn)))))

        nff
        (fn [op]
          (fn [i p]
            (if (seq? (:notes p))
              ;; find the highest
              (let [n (:notes p)
                    mn (map th/name-to-midinote n)
                    top (apply max mn)
                    r   (op #(= (th/name-to-midinote %) top) n)
                    ]
                (assoc p :notes r))
              p)))
        
        rh-top-note
        (-> rh-orig
            (ls/transform-note-payload (nff filter)))

        rh-other-notes 
        (-> rh-orig
            (ls/transform-note-payload (nff remove))
            (->> (mapcat (fn [c]
                           (let [p (i/item-payload c)
                                 new-item
                                 (fn [i n]
                                   (-> (i/identity-item-transformer c)
                                       (i/add-transform :payload (constantly (assoc p :notes [n])))
                                       (i/add-transform :beat (comp (partial + (* 0.02 (inc i))) i/item-beat))
                                       ))
                                 ]
                             (if (seq? (:notes p))
                               (map-indexed  new-item (:notes p))
                               [c])
                             )
                           ))))


        rh
        (<*>
         (-> rh-top-note (ls/amplify 1.07))
         (-> rh-other-notes (ls/amplify 0.85)))
        
        lh
        (let [ls (lily orig-piano-lh :relative :c3)]
          (->
           (<*> ls
                (-> ls (ls/transpose -12)))
           (ls/hold-for-pct 0.98)
           ((fn [s] (apply ls/line-segment-dynamics (concat [s] xpand-dyn))))
           (ls/amplify 0.82)
           )
          )

        ]
    
    (->
     (<*>
      rh
      lh
      (apply ls/pedal-held-and-cleared-at (mapcat (fn [i] [(* 15 i) (+ (* 15 i) 3) (+ (* 15 i) 7) (+ (* 15 i) 10)]) (range 15)))
      )

     )
    ))


;; overlayone is (r8 root 5 root o 5) 6 pattern shifting across
(def sin-bell-repeat
  (let [ph  (->
             (lily "r8 aes ees' aes, aes'16 aes' ees,8" :relative :c5)
             (ls/hold-for-pct 0.2)
             )
        ph2 (->
             (lily "r8 aes bes bes' aes aes, g16 aes g' f g,8 ees ees ees' r4" :relative :c5)
             (ls/hold-for-pct 0.2)
             )
        ;;        _ (try-out ph2 sin-bell)
        ]
    (>>>
     (rest-for (* 2 15))
     (->
      (ls/loop-n ph 10)
      (ls/line-segment-dynamics 0 0 (* 2 15) 20))
     (->
      (ls/loop-n ph 20)
      (ls/line-segment-dynamics 0 20 (* 4 15) 5)
      )
     ph2
     (rest-for (* 3 15))
     (->
      (ls/loop-n ph 19)
      (ls/line-segment-dynamics 0 5 (* 2 15) 30 (* 4 15) 0)
      )

     )
    ))

(def final-song
  (->
   (<*>
    (-> orig-piano (ls/on-instrument piano) (ls/with-clock clock))
    (-> sin-bell-repeat (ls/on-instrument sin-bell) (ls/with-clock con-clock))
    ))
  )

(def playit false)
(def player
  (when playit
    (midi-play
     final-song
     ;;:beat-zero (- (* 8 15) 1)
     ;;     :beat-end (* 11 15)

     :beat-clock con-clock
     )))

;; (agent-error player)

;;(def s (composition-kit.events.physical-sequence/stop player))

(ls/beat-length final-song)


