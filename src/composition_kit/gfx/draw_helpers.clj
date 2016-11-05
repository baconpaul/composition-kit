(ns composition-kit.gfx.draw-helpers
  (:import [java.awt.image BufferedImage])
  (:import [java.awt Image Frame])
  (:require [composition-kit.events.physical-sequence :as ps])
  )

(def resolutions
  {:res-720p [1280 720] 
   :res-half-720p [640 360] })

(defn new-image
  ([res]
   (if-let [wh (res resolutions)]
     (new-image (first wh) (second wh))
     (throw (ex-info "No such resolution key" {:key res})))
   )
  ([w h]
   (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)))


(defn draw-onto-scaled [img draw-fn s]
  "We work in a 1600x900 unit space and scale to the 16:9 resolution we actually have"
  (let [g (.createGraphics img)

        rh
        (java.awt.RenderingHints.
         java.awt.RenderingHints/KEY_ANTIALIASING
         java.awt.RenderingHints/VALUE_ANTIALIAS_ON
         )
        _
        (do
          (.setRenderingHints g rh)
          (.scale g s s)
          )
        ]
    (draw-fn g))
  img)

(defn draw-onto-unscaled [img draw-fn]
  (draw-onto-scaled img draw-fn 1))


(defn draw-onto [img draw-fn]
  "We work in a 160x90 unit space and scale to the 16:9 resolution we actually have"
  (let [w (.getWidth img)
        s (float (/ w 1600))
        ]
    (draw-onto-scaled img draw-fn s))
  )

(defn clear-with [img col]
  (draw-onto-unscaled img (fn [g]
                            (doto g
                              (.setColor col)
                              (.fillRect 0 0 (.getWidth img nil) (.getHeight img nil))))))

(defn v-gradient-clear [img col-top col-bot]
  (draw-onto-unscaled
   img
   (fn [g]
     (let [gp (java.awt.GradientPaint. 0 0 col-top 0 (.getHeight img) col-bot)]
       (doto g
         (.setPaint gp)
         (.fill (java.awt.geom.Rectangle2D$Double. 0 0 (.getWidth img) (.getHeight img)))
         ))))
  )

(defn show-image [img]
  (let [canvas
        (proxy [java.awt.Canvas] []
          (paint [g]
            (.drawImage g img 0 0 nil)))

        frame
        (doto (java.awt.Frame. "Image View")
          (.setSize (.getWidth img nil) (.getHeight img nil))
          (.setLayout (java.awt.BorderLayout.))
          (.add canvas java.awt.BorderLayout/CENTER)
          (.setVisible true)
          )
        
        _
        (doto frame
          (.addWindowListener
           (proxy [java.awt.event.WindowAdapter] []
             (windowClosing [e]
               (doto frame
                 (.setVisible false)
                 (.dispose))
               ))))
        ]
    frame
    )
  
  )

(defn movie-player
  [image-list window-title]
  (let [wh    (juxt #(.getWidth %) #(.getHeight %))
        sizes (if-let [imfn  (some fn? image-list)]
                [(wh ((first image-list)))]
                (map wh image-list)
                )
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
              (let [i2p (if (fn? img) (img) img)]
                (doto g
                  (.drawImage i2p 0 0 nil)))
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
        (doto (java.awt.Frame. window-title)
          (.setSize (+ 100 mw) (+ 100 mh))
          (.setLocation 0 300)
          (.setLayout (java.awt.BorderLayout.))
          (.add main-panel java.awt.BorderLayout/CENTER)
          (.setVisible true)
          (.toFront))

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
            (show-image (last images))
            
            ))

