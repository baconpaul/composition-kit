(ns composition-kit.gfx.draw-helpers
  (:import [java.awt.image BufferedImage])
  (:import [java.awt Image Frame])
  (:require [composition-kit.events.physical-sequence :as ps])
  )

(defn new-image [w h]
  (BufferedImage. w h BufferedImage/TYPE_INT_ARGB))

(defn draw-onto [img draw-fn]
  (let [g (.createGraphics img)
        rh
        (java.awt.RenderingHints.
         java.awt.RenderingHints/KEY_ANTIALIASING
         java.awt.RenderingHints/VALUE_ANTIALIAS_ON
         )
        _ (.setRenderingHints g rh)
        ]
    (draw-fn g))
  img)

(defn movie-player
  ([image-list] (movie-player [image-list "movie-player"]))
  ([image-list window-title]
   (let [sizes (map (juxt #(.getWidth %) #(.getHeight %)) image-list)
         mw    (apply max (map first sizes))
         mh    (apply max (map second sizes))

         selected-image  (atom 0)
         
         ;; Now make the panel which contains the slider and canvas
         slider
         (java.awt.Scrollbar. java.awt.Scrollbar/HORIZONTAL 0 1 0 (count image-list))

         play
         (java.awt.Button. ">")
         
         slider-panel
         (doto (java.awt.Panel.)
           (.setLayout (java.awt.BorderLayout.))
           (.add slider java.awt.BorderLayout/CENTER)
           (.add play java.awt.BorderLayout/WEST)
           )

         image-canvas
         (proxy [java.awt.Canvas] []
           (paint [g]
             (if-let [img (nth image-list @selected-image)]
               (doto g
                 (.drawImage img 0 0 nil))
               (doto g
                 (.setColor java.awt.Color/BLACK)
                 (.fillRect 0 0 (.getWidth this) (.getHeight this))
                 )
               )
             ))
         
         main-panel
         (doto (java.awt.Panel.)
           (.setLayout (java.awt.BorderLayout.))
           (.add slider-panel java.awt.BorderLayout/NORTH)
           (.add image-canvas java.awt.BorderLayout/CENTER)
           )
         
         frame
         (doto (java.awt.Frame. "show image")
           (.setSize (+ 100 mw) (+ 100 mh))
           (.setLocation 0 300)
           (.setLayout (java.awt.BorderLayout.))
           (.add main-panel java.awt.BorderLayout/CENTER)
           (.setVisible true))

         set-image!
         (fn [i]
           (reset! selected-image i)
           (.repaint image-canvas))
         _
         (doto frame
           (.addWindowListener
            (proxy [java.awt.event.WindowAdapter] []
              (windowClosing [e]
                (doto frame
                  (.setVisible false)
                  (.dispose))
                ))))
         _
         (doto slider
           (.addAdjustmentListener
            (proxy [java.awt.event.AdjustmentListener] []
              (adjustmentValueChanged [e]
                (set-image! (.getValue e))
                ))
            )
           )

         _
         (doto play
           (.addActionListener
            (proxy [java.awt.event.ActionListener] []
              (actionPerformed [e]
                (let [pseq (ps/new-sequence)
                      actions (map (fn [i] [(fn [t] (set-image! i)) (* 1000 (/ i 30))]) (range (count image-list)))
                      fseq (reduce (fn [s el] (apply ps/add-to-sequence s el)) pseq actions)
                      ]
                  (ps/play fseq)
                  )
                ))))

         ]
     { :frame frame :sel selected-image }
     )
   )
  )


#_(def mp (let [w 600
                h 500
                images (map
                        (fn [i]
                          (-> (new-image w h)
                              (draw-onto
                               (fn [g]
                                 (doto g
                                   (.setColor (java.awt.Color. 0 (* 2 i) 0))
                                   (.fillRect 0 0 w h)
                                   (.setColor (java.awt.Color/RED))
                                   (.drawLine 0 0 w i)
                                   (.drawLine 0 0 w (* 2 i))
                                   (.drawLine 0 0 (* 2 i) h))))
                              )
                          )
                        (range 100)
                        )]
            (movie-player images "lines")
            
            ))

