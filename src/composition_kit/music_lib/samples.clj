(ns composition-kit.music-lib.samples
  (:import [java.io File])
  (:import [javax.sound.sampled AudioInputStream AudioSystem]))

(defn clip-player [fname zero-point-in-microseconds]
  (let [strm  (AudioSystem/getAudioInputStream (File.  fname))
        clip  (AudioSystem/getClip)
        _     (.open clip strm)
        start-at  (fn [start-point-in-microseconds]
                    (.setMicrosecondPosition clip (- start-point-in-microseconds zero-point-in-microseconds))
                    (.start clip)
                    )
        stop      (fn [] (.stop clip))
        close     (fn [] (.close clip) (.close strm))
        ]
    {:start-at start-at :stop stop :close close :stop-and-close (juxt stop close)}
    )
  )

;;(def bd (clip-player "/Users/paul/Desktop/MM/Bouncedown.wav" 0))



