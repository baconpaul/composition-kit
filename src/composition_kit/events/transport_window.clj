(ns composition-kit.events.transport-window)

;; This is really just gunky UI code to make a window that shows me where I am and has a stop button.
;; The real trick is in logical to physical where we put in the updates of the beats to change the position
;; in the song which forces the UI refresh.

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

(defn format-midi-clock-position [mc]
  (let [sx (:sixteenths mc)
        bt (quot sx 4)
        sx (rem sx 4)
        fr (:frames mc)
        ]
    (str "MB:"
         (-> (java.text.DecimalFormat. "00000")
             (.format bt))
         "+"
         (-> (java.text.DecimalFormat. "0")
             (.format sx))
         "."
         (-> (java.text.DecimalFormat. "0")
             (.format fr))
         )))

(defn make-transport-window [^String window-title]
  (let [state  (atom {:time 0 :beat 0 :pbeat 0 :pct 0 :on-stop (fn [] true)})

        big-font    (java.awt.Font. "Menlo" 0 48)
        small-font  (java.awt.Font. "Menlo" 0 30)


        time-panel
        (proxy [java.awt.Canvas] []
          (paint [^java.awt.Graphics2D g]
            (let [beat-string (if (nil? (:midi-clock-position @state))
                                (format-beat (:beat @state) (:pct @state))
                                (format-midi-clock-position (:midi-clock-position @state))
                                )]
              (doto g
                (.setColor (java.awt.Color. 30 30 50))
                (.fillRect 0 0 (.getWidth ^java.awt.Canvas this) (.getHeight ^java.awt.Canvas this))

                (.setColor (java.awt.Color. 50 50 80))
                (.fillRect 0 0 (* (/ (:pct @state) 100) (.getWidth ^java.awt.Canvas this)) (.getHeight ^java.awt.Canvas this))

                (.setColor (java.awt.Color. 130 240 130))
                (.setFont big-font)
                (.drawString (^String format-time (:time @state)) 10 48)
                (.setColor (java.awt.Color. 130 130 240))
                (.drawString beat-string 10 108)                


                (.setColor (java.awt.Color. 130 130 240))
                (.fillRect 250 (- 110 (* 48 (:pbeat @state)))  10  (* 48 (:pbeat @state)))
                ))
            )
          )

        frame
        (doto (java.awt.Frame. window-title)
          (.setSize 500 175)
          (.setLayout (java.awt.BorderLayout.)))

        stop-button
        (doto  (java.awt.Button. "Stop")
          (.addActionListener
           (proxy [java.awt.event.ActionListener] []
             (actionPerformed [e]
               (when-let [f (:on-stop @state)]
                 (f)
                 )
               (doto frame
                 (.setVisible false)
                 (.dispose))
               )
             )
           )
          )
        
        panel
        (doto (java.awt.Panel.)
          (.setLayout (java.awt.BorderLayout.))
          (.add time-panel java.awt.BorderLayout/CENTER)
          (.add stop-button java.awt.BorderLayout/SOUTH))


        
        _
        (doto frame
          (.add panel java.awt.BorderLayout/CENTER)
          (.validate)
          (.setVisible true)

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
     :close  (fn [] ((:on-stop @state)) (doto frame (.setVisible false) (.dispose)))
     :on-stop (fn [f] (swap! state update :on-stop (fn [orig] (juxt f orig))))
     }
    )
  )

(defn agent-transport-window []
  (:transport @*agent*))



