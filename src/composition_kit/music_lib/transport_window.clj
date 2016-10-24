(ns composition-kit.music-lib.transport-window)

(defn make-transport-window [window-title]
  (let [state  (atom {:beat 0 :time 0 :measure-beat (0 0)})

        canvas
        (proxy [java.awt.Canvas] []
          (paint [g]
            (doto g
              (.setColor (java.awt.Color. 30 30 50))
              (.fillRect 0 0 (.getWidth this) (.getHeight this))
              (.setColor (java.awt.Color. 200 0 0))
              (.drawString (str @state) 10 100)
              )
            )
          )
        
        frame
        (doto (java.awt.Frame. window-title)
          (.setSize 400 200)
          (.setLayout (java.awt.BorderLayout.))
          (.add canvas java.awt.BorderLayout/CENTER)
          (.validate)
          (.setVisible true))
        _
        (doto frame
          (.addWindowListener
           (proxy [java.awt.event.WindowAdapter] []
             (windowClosing [e]
               ;; Get off the AWT thread
               ;;(send (agent {}) (fn [v] (.stop animator)))

               (.setVisible frame false)
               (.dispose frame)

               ))))

        assoc-state
        (fn [k v]
          (swap! state assoc k v)
          (.repaint canvas))

        ]
    {:canvas canvas
     :state  state
     :assoc  assoc-state}
    )
  )

;;(def w (make-transport-window "hiya 32"))
;;((:assoc w) :foobaz 4)
;;((:assoc w) :time 2)




