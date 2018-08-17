(ns composition-kit.events.physical-sequence
  (:require [composition-kit.events.transport-window :as tw])
  (:require [composition-kit.music-lib.midi-util :as midi])
  )

;; This is a set of functions which allow you to build a sequence of functions then 'play' them (have them triggered at their time)
;; once built. It's not really intended for adding events on live but I suppose you could do that, since it would work.
;;
;; The implementation is done by making the sequence 'object' an agent with a sorted set to hold the notes, sorted by time of
;; the item. We may want to sort by time then message type later. Lets see.


(defn ^:private compare-by-key-then [k]
  (fn [c1 c2]
    (let [comp  (compare (k c1) (k c2))]
      (if (zero? comp)
        (reduce (fn [s n] (if (zero? s) n s) )
                (map (fn [[k v]]
                       (if (and (instance? Comparable (first v)) (instance? Comparable (second v)))
                         (compare (first v) (second v))
                         (compare (hash (first v)) (hash (second v)))))
                     (merge-with (fn [& v] v) c1 c2))
                )
        comp)))
  )


(defn new-sequence []
  "Generate a new sequence object to which sequencable items can be added.
This is the data which is bound by the agent when you play"
  {:seq (sorted-set-by (compare-by-key-then :time))
   :return-values []
   :play true
   :transport nil })


(defn add-to-sequence [s & items]
  (if (not (zero? (mod (count items) 2)))
    (throw (ex-info "Unable to find message/time pairs" {:items items}))
    (let [item-maps (map (fn [ i t ] { :item i :time t }) (take-nth 2 items) (take-nth 2 (rest items)))
          new-set   (reduce conj (:seq s) item-maps)]
      (assoc s :seq new-set))))


;;; OK to play in either standalone or slave mode we need to modify this so that t0-in-milis and currenttime aren't
;;; just blatted in; but rather the agent data gets a toffset and toffset-0 and we also have a defered state, then we
;;; have a separate midi input reader update the agent data with SMTPE timecode and play and stop. So write down that
;;; state diagram and have the first change be that the state diagram work in no-slaved mode; then add a ps/play-slaved
;;; and in code a midi-play-slaved and work on that with the timecode stuff in the midi lib.

(defn ^:private play-on-thread [agent-data]
  ;; this function schedules the next event and gets a little spinny as the event comes near so we make
  ;; sure we hit the milisecond accuracy. It also allows you to stop the playing as you go
  ;; The basic protocol is to poll the seq which is time ordered and sleep with a backoff
  ;; but if I'm empty or have my play set to falls externally, stop and call the user shutdowns
  (let [rnow              (- (System/currentTimeMillis) (:t0-in-millis agent-data))
        to-be-played      (:seq agent-data)
        curr              (take-while #(<= (:time %) rnow) to-be-played)
        future-item       (drop-while #(<= (:time %) rnow) to-be-played)
        t-w               (:transport agent-data)
        ]
    (when (and t-w (:assoc t-w)) ((:assoc t-w ) :time rnow))
    (cond
      (empty? to-be-played)    (do
                                 (when t-w ((:close t-w)))
                                 (assoc agent-data :play false)
                                 )

      (not (:play agent-data)) (do
                                 agent-data) ;; someone stopped me in another action so just chillax

      (not (empty? curr)   )   (do
                                 (send *agent* play-on-thread)
                                 (reduce
                                  (fn [data curr-el] ;; evaluate the item and glie it onto the return values
                                    (let [return-value ((:item curr-el) rnow)
                                          return-values (conj (:return-values data) return-value)
                                          ]
                                      (-> data
                                          (assoc :return-values return-values))))
                                  (assoc agent-data :seq future-item) ;; onto agent data which has rest attached
                                  curr) ;; for each one we stripped off
                                 )

      :else                    (let [time-until  (- (:time (first to-be-played)) rnow)]
                                 ;; spin (with a little backoff sleeping)
                                 (send *agent* play-on-thread)
                                 (when (> time-until 5) (Thread/sleep (min 100 (* time-until 0.7)))) ;; that 100 keeps clock tickin
                                 agent-data)
      )
    )
  )

(defn stop [ s ] (send s (fn [agent-data] (assoc agent-data :play false))))


;;; These are the state transitions for the slaved agent model
;;;
;;; if (:master == :smpte)
;;;    look at :slave-status
;;;    :awaiting-transport   -> means either we got a :stop or have never started and are waiting for a :start or :continue
;;;    :awaiting-first-time  -> means we got a :start or a :continue but haven't had a time yet
;;;    :in-transport         -> we have a start and are getting timecodes
;;;          :t0-in-milis has standard setup
;;;          inbound timecodes update that
;;;
;;; So what do our transitions look like
;;;    :awaiting-transport   -> :start or :continue -> :awaiting-first-time; all other are errors or ignores
;;;    :awaiting-first-time  -> :time -> :in-transport with t0 set; :stop -> :awaiting-transport; :continue is fine; :start is errors
;;;    :in-transport         -> :time -> update :t0; :stop -> :awaiting-transport; :continue -> no-op; :start is an error

(defn make-smtpe-transition-engine [ag]
  (fn [action data]
    (let [master       (:master @ag)
          slave-status (:slave-status @ag)
          start-fn     (fn [a]
                         (-> a
                             (assoc :play true)
                             (assoc :seq (:original-seq a))
                             (assoc :slave-status :awaiting-first-time))
                         )
          ]
      (if (not (or (= master :midi-clock-position))) (println action master))
      (if (= master :smtpe)
        (condp = action
          :start
          (do
            (send ag start-fn))

          :continue
          (do
            (send ag start-fn))

          :stop
          (do
            ;; We actually need to do quite a lot of state resetting here
            (send ag
                  (fn [a]
                    (when-let [stop-fn (:user-stop (:args a))]
                      (send *agent* (fn [x] (stop-fn) x))
                      )
                    (-> a
                        (assoc :play false)
                        (assoc :slave-status :awaiting-transport)
                        )
                    ))
            )

          :smtpe-timecode-time
          (let [t data]
            (send ag
                  (fn [a]
                    (if (= (:slave-status a) :awaiting-first-time)
                      (do
                        (send *agent* play-on-thread))
                      )
                    (-> a
                        ;; FIXME we want to trim events here up to time
                        (assoc :t0-in-millis (- (System/currentTimeMillis) t))
                        (assoc :slave-status :in-transport)
                        )
                    ))
            )
          
          nil  ;; We do hae some unhandled states
          )
        nil
        )
      )
    )
  )

(defn setup-agent [s & items]
  (let [args  (apply hash-map items)
        use-transport true
        mod-s
        (if use-transport
          (let [t-w (tw/make-transport-window "Transport")]
            (assoc s :transport t-w))
          s)
        ag   (agent (->  mod-s
                         (assoc :args args)
                         (assoc :original-seq (:seq mod-s))))
        ]
    (when use-transport
      ((:on-stop (:transport @ag))
       (fn []
         (do
           (stop ag)
           (when-let [us (:user-stop args)]
             (send ag (fn [d] (us) d))))
         )
       )
      )
    ag
    )
  )

(defn play [ s & items ]
  (let [ag (apply setup-agent s items)]
    (send ag (fn [ad]
               (send *agent* play-on-thread)
               (-> ad
                   (assoc :master :native)
                   (assoc :t0-in-millis (System/currentTimeMillis)))
               )))
  )


(defn play-slaved [ s bus & items ]
  (let [ag (apply setup-agent s items)
        buso (midi/get-opened-transmitter bus)
        ag-eh (fn [a e]          (println "play agent occured: " e " and we still have value " (-> @a (dissoc :seq) (dissoc :original-seq))))
        _ (set-error-handler! ag ag-eh)
        ]
    ;; SetUp SMTPE Reciever on bus
    (midi/register-transmitter-callback buso (midi/make-time-code-interpret (make-smtpe-transition-engine ag)))

    ;; Let the agent know what's up with what
    (send ag (fn [ad]
               ;; We don't play here; we play when we are ready (send *agent* play-on-thread)
               (-> ad
                   (assoc :master :smtpe)
                   (assoc :slave-status :waiting-transport)
                   )
               
               )))
  )


