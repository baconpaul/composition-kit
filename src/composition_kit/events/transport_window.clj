(ns composition-kit.events.transport-window)

(defn format-time [t]
  (let [ms (mod t 1000)
        ss (int (/ t 1000.0))
        s  (mod ss 60)
        m  (int (/ ss 60))
        ]
    ;;[ ms s m ]
    (str
     "T :"
     (-> (java.text.DecimalFormat. "00")
         (.format m))
     ":"
     (-> (java.text.DecimalFormat. "00")
         (.format s))
     ":"
     (-> (java.text.DecimalFormat. "000")
         (.format ms))
     )

    )
  )

(defn format-beat [b p]
  (str "B :"
       (-> (java.text.DecimalFormat. "00000")
           (.format b))
       " /"
       (-> (java.text.DecimalFormat. "00.0")
           (.format p))
       "%"
       ))

(defn make-transport-window [window-title]
  (let [state  (atom {:time 0 :beat 0 :pbeat 0 :pct 0 :on-stop nil})

        big-font    (java.awt.Font. "Menlo" 0 48)
        small-font  (java.awt.Font. "Menlo" 0 30)

        time-panel
        (proxy [java.awt.Canvas] []
          (paint [g]
            (doto g
              (.setColor (java.awt.Color. 30 30 50))
              (.fillRect 0 0 (.getWidth this) (.getHeight this))

              (.setColor (java.awt.Color. 50 50 80))
              (.fillRect 0 0 (* (/ (:pct @state) 100) (.getWidth this)) (.getHeight this))

              (.setColor (java.awt.Color. 130 240 130))
              (.setFont big-font)
              (.drawString (format-time (:time @state)) 10 48)
              (.setColor (java.awt.Color. 130 130 240))
              (.drawString (format-beat (:beat @state) (:pct @state)) 10 108)

              (.setColor (java.awt.Color. 130 130 240))
              (.fillRect 250 (- 110 (* 48 (:pbeat @state)))  10  (* 48 (:pbeat @state)))
              )
            )
          )


        stop-button
        (doto  (java.awt.Button. "Stop")
          (.addActionListener
           (proxy [java.awt.event.ActionListener] []
             (actionPerformed [e]
               (when-let [f (:on-stop @state)]
                 (f)
                 )
               )
             )
           )
          )
        
        panel
        (doto (java.awt.Panel.)
          (.setLayout (java.awt.BorderLayout.))
          (.add time-panel java.awt.BorderLayout/CENTER)
          (.add stop-button java.awt.BorderLayout/SOUTH))


        
        frame
        (doto (java.awt.Frame. window-title)
          (.setSize 500 175)
          (.setLayout (java.awt.BorderLayout.))
          (.add panel java.awt.BorderLayout/CENTER)
          (.validate)
          (.setVisible true))
        _
        (doto frame
          (.addWindowListener
           (proxy [java.awt.event.WindowAdapter] []
             (windowClosing [e]
               ;; Get off the AWT thread
               (when-let [us (:on-stop @state)]
                 (send (agent {}) (fn [v] (us) v)))

               (doto frame
                 (.setVisible false)
                 (.dispose))

               ))))

        assoc-state
        (fn [k v]
          (swap! state assoc k v)
          (.repaint time-panel))

        ]
    {:panel  panel
     :state  state
     :assoc  assoc-state
     :close  (fn [] (doto frame (.setVisible false) (.dispose)))
     :on-stop (fn [f] (swap! state assoc :on-stop f))
     }
    )
  )

(defn agent-transport-window []
  (:transport @*agent*))

;;(def w (make-transport-window "ff"))
;;((:close w))

