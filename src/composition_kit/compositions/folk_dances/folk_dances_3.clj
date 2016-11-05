(ns composition-kit.compositions.folk-dances.folk-dances-3
  (:require [composition-kit.music-lib.midi-util :as midi])
  (:require [composition-kit.music-lib.tempo :as tempo])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.logical-item :as i])

  (:use composition-kit.core))

;; TODOS
;; - Bells in middle section

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
(def pad (midi/midi-instrument 2))
(def chorus (midi/midi-instrument 3))
(def ring-bells (midi/midi-instrument 4))
(def clock (near-constant-tempo 15 4 95))
(def con-clock (tempo/constant-tempo 15 4 95))


(defn try-out [p i]
  (-> p (ls/on-instrument i) (ls/with-clock clock) (midi-play :beat-clock con-clock)))


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
      (apply ls/pedal-held-and-cleared-at
             (mapcat (fn [i] [(* 15 i) (+ (* 15 i) 3) (+ (* 15 i) 7) (+ (* 15 i) 10)]) (range 15))
             )
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

        ph3 (-> (lily "r8 ees8 ees16 ees' ees' ees,16" :relative :c5)
                (ls/hold-for-pct 0.2)
                (ls/loop-n 20)
                (ls/line-segment-dynamics 0 10 40 10)
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

     (rest-for (* 3 15))
     ;;ph3

     (->
      (ls/loop-n ph 18)
      (ls/line-segment-dynamics 0 5 (* 2 15) 30 (* 4 15) 0)
      )
     (->
      (lily "r8 aes16 aes' bes8 g, aes" :relative :c5)
      (ls/hold-for-pct 0.2)
      (ls/explicit-dynamics '(0 21 26 28 21 17))
      )

     )
    ))

(def pad-phrase
  (>>>
   (rest-for 30)
   (-> (ls/explicit-phrase [  [:des3 :aes3] [:c3 :g3] [:c3 :aes3]] [  15 7 8])
       (ls/explicit-dynamics '(20 21 24 31))
       (ls/hold-for-pct 0.99)
       )
   (-> (ls/explicit-phrase [  [ :c3 :aes3 ] [:c3 :aes3 ] [:des3 :aes3] [:c3 :g3] [:c3 :aes3]] [15 15  15 7 8])
       (ls/explicit-dynamics '(20 21 24 31))
       (ls/hold-for-pct 0.99)
       )
   (-> (ls/explicit-phrase [[ :des3 :ees3] [ :ees3 :bes3 :c4]
                            [:c3 :aes3] [:des3 :aes3]  [:g3 :ees4] ] [ 3 4 3 3 2 ])
       (ls/explicit-dynamics '(30 34 40 44 46 ))
       (ls/hold-for-pct 0.99)
       )
   (-> (ls/explicit-phrase [[ :ees3 :aes3 :bes3]
                            [ :ees3 :aes3 :g3]
                            [ :ees3 :aes3 :bes3]
                            [ :ees3 :aes3 :g3]
                            [ :c3 :c4]
                            ]
                           [ 3  4 3 3 2 ])
       (ls/explicit-dynamics '(30 34 40 44 46 ))
       (ls/hold-for-pct 0.99)
       )

   (-> (ls/explicit-phrase [[ :des3 :aes3 :c4] [:des3 :aes3 :bes3 ]
                            [:des3 :aes3 :c4] ] [3 4 1])
       (ls/explicit-dynamics '(47 11 21 ))
       (ls/hold-for-pct 0.99)
       )
   (rest-for 7)

   (-> (ls/explicit-phrase [ [ :c3 :aes3] [:c3 :aes3] [:des3 :aes3] [:c3 :g3] [:c3 :aes3]] [15 15 15 7 8])
       (ls/explicit-dynamics '(20 21 24 31))
       (ls/hold-for-pct 0.99)
       )
   ))

(def choral-one
  (let [
        p1  (->  (lily "c4 des ees f g aes c bes8 r8 c, des ees f g aes bes2 aes8 r8 r4" :relative :c4)
                 (ls/hold-for-pct 1.01)
                 (ls/line-segment-dynamics 0 40 6 55 7 45 15 62)
                 )
        p2  (->  (lily "ees4 f g aes bes c aes g8 r8 c, f f aes bes c des2 c8 r8 r4" :relative :c3)
                 (ls/hold-for-pct 1.01)
                 (ls/line-segment-dynamics 0 40 6 55 7 45 15 62)
                 )

        
        p1-alt  (->  (lily "c4 des ees f g aes c bes8 r8 c, des ees f aes2" :relative :c4)
                     (ls/hold-for-pct 1.01)
                     (ls/line-segment-dynamics 0 40 6 55 7 45 15 12)
                     )
        p2-alt  (->  (lily "ees4 f g aes bes c aes g8 r8 c, f f aes ees2" :relative :c3)
                     (ls/hold-for-pct 1.01)
                     (ls/line-segment-dynamics 0 40 6 55 7 45 15 12)
                     )

        p3        (->  (lily "des4 ees f g aes bes ces bes8 r8 ces, des ees f g aes ces2 c8 r8 r4" :relative :c4)
                       (ls/hold-for-pct 1.01)
                       (ls/line-segment-dynamics 0 50 6 59 7 51 15 67)
                       )

        p4  (->  (lily "bes4 ces des ees f g aes g8 r8 ees f g aes bes ces ces2 c8 r8 r4" :relative :c3)
                 (ls/hold-for-pct 1.01)
                 (ls/line-segment-dynamics 0 50 6 59 7 51 15 67)
                 )

        alto-mid (-> (lily "bes4 aes ees8. r16  ees2.. r8 aes4 g c, des2. ees2
ees2. ees1 ees2. ees2. bes'2 c4 c c bes1 c4 c c r2. r2
" :relative :c5)
                     (ls/hold-for-pct 0.99)
                     )

        tenor-mid (-> (lily "des2. ees4 f g bes aes2. des,4 aes des f aes 
g ees bes c1 des4 ees f g aes bes c2 des2. c1 des2. r2. r2" :relative :c3)
                      (ls/hold-for-pct 0.99))


        ]
    
    ;;(try-out (<*> alto-mid tenor-mid) chorus)
    (>>>
     (rest-for (* 4 15))
     (<*> p1 p2)
     (<*> p1 p2)
     (<*> p3 p4)
     (<*> p1 p2)

     (<*>  alto-mid tenor-mid)
     
     (<*> p1 p2)
     (<*> p1 p2)
     (<*> p3 p4)
     (<*> p1-alt p2-alt);; alternate here please!
     )
    ))

(def glock
  (let [alto-mid (-> (lily "bes4 aes ees8. r16  ees4 f4 g bes aes4 g c, des2. ees2
g4 ees bes c1 des4 ees f g aes bes bes2 c,4 c c bes1 c4 c c des2. r2
" :relative :c5)
                     (ls/hold-for-pct 0.2)
                     (ls/line-segment-dynamics 0 50 6 59 7 51 15 67)

                     (as-> ls
                         (<*>
                          ls
                          (-> ls
                              (ls/amplify 0.2)
                              (ls/transpose 7)
                              (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.0525)))
                              )
                          (-> ls
                              (ls/amplify 0.4)
                              (ls/transpose 12)
                              (ls/transform :beat (fn [i] (+ (i/item-beat i) 0.107)))
                              )
                          ))
                     (ls/amplify 0.3)
                     )
        
        ]
    #_(try-out alto-mid ring-bells)
    (>>>
     (rest-for (* 8 15))
     alto-mid
     )
    )
  )

(def final-song
  (->
   (<*>
    (-> orig-piano (ls/on-instrument piano) (ls/with-clock clock))
    (-> sin-bell-repeat (ls/on-instrument sin-bell) (ls/with-clock con-clock))
    (-> pad-phrase (ls/on-instrument pad ) (ls/with-clock con-clock))
    (-> choral-one (ls/on-instrument chorus) (ls/with-clock clock))
    (-> glock (ls/on-instrument ring-bells) (ls/with-clock clock))
    ))
  )

(def playit false)
(def player
  (when playit
    (midi-play
     final-song
     :beat-zero -1
     ;;:beat-zero (- (* 8 15) 1)
     ;;:beat-end (* 12 15)

     :beat-clock con-clock
     )))

;; (agent-error player)

;;(def s (composition-kit.events.physical-sequence/stop player))

(ls/beat-length final-song)


