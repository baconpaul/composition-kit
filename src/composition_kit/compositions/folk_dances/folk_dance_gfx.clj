(ns composition-kit.compositions.folk-dances.folk-dance-gfx
  (:require [composition-kit.gfx.draw-helpers :as dh])
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  )

(defn ellipse-star [img x y size smudge]
  (dh/draw-onto
   img
   (fn [g]
     (doseq [r  (reverse  (range size))]
       (let [rnorm (/ r size)
             rneg  (- 1 rnorm)
             rneg-255 (int (* 255 rneg))

             alph  (int  (min 255 (+ rneg-255 (* 50 smudge))))
             col   (java.awt.Color. rneg-255 rneg-255 (int (* 0.8 rneg-255)) alph)

             sb2 (/ r 2)
             x0  (- x sb2)
             y0  (- y sb2)
             ]
         (doto g 
           (.setColor col)
           (.fillOval x0 y0 r (* (inc smudge) r)))
         )
       )
     
     
     )
   )
  )

#_(def star-db
    (with-open [in-file (io/reader "/Users/paul/dev/HYG-Database/hygdata_v3.csv")]
      (doall
       (csv/read-csv in-file))))


;; (def star-db nil)
;; (def star-schema (first star-db))
;; (def star-data (rest star-db))

;; (filter #(= (first %) "dec") (map vector star-schema (range)))
;; (count  (filter #(> % 13) (map #(Double/parseDouble  (nth % 13)) star-data)))

;; (def bright-stars (filter #(> (Double/parseDouble (nth % 13)) 10) star-data))

;; ;; ra is 0->24
;; ;; de is -90 -> 90

;; (def bright-ra-de-ma (map (juxt #(Double/parseDouble (nth % 7)) #(Double/parseDouble (nth % 8)) #(Double/parseDouble (nth % 13))) bright-stars))

;; (defn gen-for [n]
;;   (fn []
;;     (-> (dh/new-image :res-720p)
;;         (dh/clear-with java.awt.Color/BLACK)
;;         (dh/draw-onto
;;          (fn [g]
;;            (let [sf (fn [[ra de ma]]
;;                       [ (* (/ ra 24) 1600) (* (/ (+ de 90) 180) 900)]
;;                       )]
;;              (doseq [coord (take-nth n  bright-ra-de-ma)]
;;                (let [sc (sf coord)
;;                      size (/  (-  (last coord) 7) 2)
;;                      ]
;;                  (doto g
;;                    (.setColor java.awt.Color/WHITE)
;;                    (.fillOval (first sc) (second sc) size size))
;;                  )
;;                )
;;              )
;;            ))
;;         )))

;; (defn state-els-for [n m]
;;   (let [sf (fn [[ra de ma]]
;;              [ (* (/ ra 24) 1600) (* (/ (+ de 90) 180) 900)]
;;              )]
;;     (for [coord (take-nth n (drop m bright-ra-de-ma))]
;;       (let [sc (sf coord)
;;             size (last coord)
;;             ]
;;         (concat sc [ size ])
;;         )
;;       )
;;     )
;;   )



;; ;;(state-els-for 11               )

;; ;; So how do I want to structure a 'video program'. Well clearly I have a state which evolves at 30FPS
;; ;; and a renderer for that state and moments when I inject changes into that state. So that means my basic data
;; ;; structure is something like

;; (defn state-stepper [evolution-fn dt]
;;   (fn [ s & state-injectors ] ;; state injectors is [ (fn state -> state)
;;     (let [s-post (reduce (fn [s f] (f s)) s state-injectors)]
;;       (evolution-fn s-post dt)
;;       )
;;     )
;;   )

;; ;; OK so how would I use this to make shrinking ellipses. Well I'd start with a state-zero fn


;; ;; And an evolver
;; (defn star-evolve [s dt]
;;   (case (:mode s)
;;     :grow
;;     (update s :stars #(map (fn [ [x y s] ] [ x y (+ (* (- s 20) 0.91 ) 20)]) %))

;;     :smudge
;;     (update s :smudge (partial + 0.4))
;;     )
;;   )

;; ;; and a mode swapper
;; (defn mode-swap [s] (assoc s :mode :smudge))
;; (defn add-stars [s] (assoc s :stars (state-els-for 35 1)) )
;; (defn add-more  [s] (update s :stars concat (state-els-for 35 2)) )

;; ;; Now make a command sequence
;; (def state-cmds (concat [ add-stars ] (repeat 30 nil) [add-more ] (repeat 30 nil) [ mode-swap] (repeat 30 nil)))

;; ;; And now lets generate the states from the commands
;; (defn gen-states [ s0 commands stepper]
;;   (rest (reductions (fn [s cmd]
;;                       (if cmd
;;                         (stepper s cmd)
;;                         (stepper s)
;;                         )
;;                       )
;;                     s0
;;                     commands))
;;   )

;; ;; And wire it together
;; (def star-states  (gen-states { :stars [] :smudge 0 :mode :grow } state-cmds (state-stepper star-evolve 0.01)))

;; ;; Now make a renderer
;; (defn star-state-renderer [s]
;;   (fn []
;;     (let [img (dh/new-image :res-half-720p)
;;           mode (:mode s)
;;           smudge (:smudge s)
;;           stars (:stars s)
;;           ]
;;       (case mode
;;         :grow
;;         (do
;;           (dh/v-gradient-clear img java.awt.Color/BLACK (java.awt.Color. 10 10 200 ))
;;           (doseq [s stars]
;;             (apply ellipse-star img (concat s [ smudge]))
;;             )
;;           )
;;         :smudge
;;         (do
;;           (dh/v-gradient-clear img java.awt.Color/BLACK (java.awt.Color. 10 10 (int (- 200 (* 3 smudge))) ))
;;           (doseq [s stars]
;;             (apply ellipse-star img (concat s [ smudge]))
;;             )
;;           )
;;         )

;;       img
;;       )
;;     )
;;   )

;; ;;(dh/movie-player )


;; (dh/movie-player  (map #(%) (map star-state-renderer star-states)) "stars")



;;(dh/show-image ((gen-for 7)))

#_(-> (dh/new-image 400 400)
      (ellipse-star 170 170 50 0 1)
      (dh/show-image))

#_(let [elf (fn [i] (fn [] (let [img (dh/new-image :res-half-720p)
                                 ]
                             ;;(dh/clear-with img java.awt.Color/BLACK)
                             (dh/v-gradient-clear img java.awt.Color/BLACK (java.awt.Color. 10 10 (int  (- 100 (inc i))) ))
                             (ellipse-star img 170 170 10 (/ i 3))
                             (ellipse-star img 240 130 6 (/ i 3))
                             (ellipse-star img 330 190 8 (/ i 3))
                             img
                             ))
              )
        imgs (map elf (range 50))
        ]
    (dh/movie-player imgs "smudge")
    ;;(dh/show-image ((first imgs)))
    )

