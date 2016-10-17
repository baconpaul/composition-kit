(ns composition-kit.music-lib.parse
  (:require [composition-kit.music-lib.logical-sequence :as ls])
  (:require [composition-kit.music-lib.tonal-theory :as th])
  (:require [instaparse.core :as insta])
  (:require [instaparse.failure :as instafail]))

(def lily-phrase-grammar
  "
  l-expression = <whitespace*> ( l-voice | l-voices ) 
                (<whitespace>  ( l-voice | l-voices ))*
                 <whitespace*>
  l-voice = (l-braces | l-tuplet | l-note-item)

  l-voices = <'<<'> <whitespace*> l-voice+ <whitespace*> 
                 ( <'\\\\\\\\'> <whitespace+> l-voice+ <whitespace*> )* <'>>'>

  l-braces = <'{'> <whitespace?> l-expression <whitespace?> <'}'>

  l-tuplet = <'\\\\tuplet'> <whitespace?> l-fraction <whitespace?> l-braces
  l-fraction = #\"\\d+\" <'/'> #\"\\d+\"

  l-note-item = l-note-with-duration | l-chord
  
  l-note-with-duration = l-note l-duration?

  l-note = l-note-name l-accidental? l-octave-modifier?

  l-note-name = ('a'|'b'|'c'|'d'|'e'|'f'|'g'|'r') (* l-rest)
  l-rest = 'r' *)
  l-accidental = ('is'|'iis'|'es'|'ees')
  l-octave-modifier = (','+|'\\''+)

  l-chord = <'<'> <whitespace*> l-note (<whitespace> l-note)+ <whitespace*> <'>'> l-duration?

  l-duration = #\"\\d+\" l-duration-modifier*
  l-duration-modifier = '.' | '..' | '...'

  whitespace = #'\\s+'

  "
  )

(def lily-phrase-parser (insta/parser lily-phrase-grammar))

(defn duration-list-to-beats [ ld ]
  (let [ndur (first ld)
        rval (rest ld)
        _    (when (and (not (empty? rval)) (not (= (ffirst rval) :l-duration-modifier)))
               (throw (ex-info "Malformed parse tree in duration"
                               {:d ld})))
        dots (if (empty? rval) "" (second (first rval)))
        ]
    (* (/ 4 (Integer/parseInt ndur))
       (reduce + 1 (take (count dots) (iterate #(/ % 2) 1/2))))
    )
  )

(defn note-with-duration-to-note [[tag & data] prior]
  "item is a parse tree item for a note-with-duration; prior is a note like :c4"
  ;; TODO: Handle accidentals!!
  
  (when-not (= tag :l-note-with-duration)
    (throw (ex-info "Can only convert notes with duration, not this" { :tag tag })))
  (let [prior-note   (th/note-by-name prior)
        prior-pitch  (:pitch prior-note)
        kl-to-m      #(reduce (fn [m el] (assoc m (first el) (rest el))) {} %)
        dat-map      (-> (kl-to-m data)
                         (update-in [:l-note] kl-to-m))

        dur          (when (:l-duration dat-map)
                       (duration-list-to-beats (:l-duration dat-map)))
        l-note        (:l-note dat-map)
        nn            (first (:l-note-name l-note))
        ;; a "rest" has the same pitch
        is-rest       (= nn "r")
        pitch         (if is-rest prior-pitch (keyword (str nn (or (first (:l-accidental l-note)) ""))))

        ac            (or (first (:l-accidental l-note)) "")
        om            (apply str (or (:l-octave-modifier l-note) ""))

        ninterval     (let [i (th/interval-between prior-pitch pitch)]
                        (if (< i 7) i (- i 12)))

        roctavediff   (* (count om) (if (= (first om) \,) -1 1))

        interval      ( + ( * 12 roctavediff) ninterval )
        notes         (th/notes-by-midinote (+ interval (:midinote prior-note)))
        note          (first (filter #(= (:pitch %) pitch) notes))
        ;; TODO: Rests
        
        ]
    {:is-rest is-rest :note note :dur dur}
    )
  )

(def lily-blank-state   {:raw-music []
                         :notes     []
                         :durations []
                         :logical-sequence []
                         :starts-at  0
                         :prior-dur   0
                         :prior-root :c4})

;; Darn it - since the LS changes between a list and vector unpredictably just do this whack together of vectors.
(defn conj-on [m k i] (update-in m [k] concat [i]))

(defn lily-phrase-traverse
  [parse-item state]
  (let [[key & nodes] parse-item]
    (case key
      :l-expression    (->
                        (reduce (fn [s v] (lily-phrase-traverse v s)) state nodes)
                        (update-in [:logical-sequence] ls/concrete-logical-sequence))
      :l-voice         (lily-phrase-traverse (first nodes) state) ;; simple passthrough
      :l-note-item     (lily-phrase-traverse (first nodes) state)

      :l-note-with-duration
      (let [new-note  (note-with-duration-to-note parse-item (:prior-root state))
            dur       (or (:dur new-note) (:prior-dur state))
            ]
        (-> state
            (conj-on :raw-music parse-item)
            (conj-on :notes     (when-not (:is-rest new-note) (:note new-note)))
            (conj-on :durations (:dur new-note))
            (conj-on :logical-sequence
                     (if (:is-rest new-note)
                       (ls/rest-with-duration dur (:starts-at state))
                       (ls/notes-with-duration (:note (:note new-note)) dur (:starts-at state) 0.95)))
            (update-in [:starts-at] #(+ % dur))
            (assoc   :prior-root (:note (:note new-note)))
            (assoc   :prior-dur  dur)
            ))

      :l-chord
      (let [notes  (filter #(= (first %) :l-note) nodes)
            kl-to-m      #(reduce (fn [m el] (assoc m (first el) (rest el))) {} %)
            other  (kl-to-m (filter #(not (= (first %) :l-note)) nodes))

            resolved-chord-notes
            (loop [[n & rst] notes
                   p         (:prior-root state)
                   res       [] ]
              (if (nil? n) res
                  (let [new-note (note-with-duration-to-note [ :l-note-with-duration n ] p)]
                    (recur rst (:note (:note new-note)) (conj res (:note new-note))))))
            
            dur
            (if (:l-duration other)
              (duration-list-to-beats (:l-duration other))
              (:prior-dur state))
            
            ]
        (-> state
            (conj-on :raw-music parse-item)
            (conj-on :notes     resolved-chord-notes)
            (conj-on :durations dur)
            (conj-on :logical-sequence
                     (ls/notes-with-duration (map :note resolved-chord-notes) dur (:starts-at state)))
            (update-in [:starts-at] #(+ % dur))
            (assoc :prior-root (:note (first resolved-chord-notes)))
            (assoc   :prior-dur  dur)))

      :l-voices
      ;; This is actually pretty straigt forward. Make a blank state with the prior of the current state
      ;; then start mapping it over the voices
      (let [bstate    (-> lily-blank-state
                          ;;(assoc :starts-at (:starts-at state))

                          (assoc :prior-dur  (:prior-dur state)))
            resolved  (map #(lily-phrase-traverse % bstate) nodes)

            merged    (apply ls/merge-sequences (map :logical-sequence resolved))
            nextls    (ls/concat-sequences (:logical-sequence state) merged)
            nextsa    (ls/beat-length nextls)]
        (-> state
            (conj-on :raw-music parse-item)
            (conj-on :notes :VOICES)
            (conj-on :dur (ls/beat-length merged))
            (assoc :logical-sequence nextls)
            (assoc :starts-at nextsa))
        )

      :l-braces
      ;; reduce the state across
      (->
       (reduce (fn [s n] (lily-phrase-traverse n s)) state nodes)
       (update-in [:logical-sequence] ls/concrete-logical-sequence))
      )
    )
  )

(defn lily-to-logical-sequence
  [line & optarr ]
  (let [opt    (apply hash-map optarr)
        state  (-> lily-blank-state
                   (assoc :prior-root (or (:relative opt) (:prior-root lily-blank-state))))
        parse  (lily-phrase-parser line)
        _      (if (insta/failure? parse)
                 (throw (ex-info (str "Failed to parse " (with-out-str (instafail/pprint-failure parse)))
                                 { :failure parse :input line } )))
        res    (lily-phrase-traverse parse state)]
    (:logical-sequence res)
    )
  )

(defn char-to-range
  "given a character make a midi range with semantic that 0-9 and
  a-z both map to min to max accordingly"
  ([c]  (char-to-range c 0 127))
  ([c min max]
   (let  [within  (fn [l h] (and (>= 0 (compare l c)) (>= 0 (compare c h))))
          spread  (* 1.0 (- max min))
          val     (fn [z r]
                    (let [cz   (- (int c) (int z))
                          delt (/ spread (dec r))]
                      (int (+ (* cz delt) min))))
          ]
     (cond
       (within \0 \9) (val \0 10)
       (within \a \z) (val \a 26)
       (within \A \Z) (val \A 26)
       true           (int (+ (/ spread 2) min))))))

(defn str->n
  "Given a single line of a sequence like

   'X...X...X.X.X...'

  and a note name (like :c1) return a logical sequence with the pattern inserted.

  Additional arguments include  
    :dur   (to have a different note duration; the default is 1/16)
    :xform (to have a different char-to-range)
    :item-factory (to make something other than notes with duration)
  
  (str->n :c2 \"x...a...x...y.z.\" :dur 1/32) for 32n/d notes
  "
  ([target line & restp]
   (let [restmap  (apply hash-map restp)
         xform    (or (:xform restmap) char-to-range)
         dur      (or (:dur restmap) 1/4)] ;; 1/4 of a beat is a 16th note generally
     (->>
      (map-indexed (fn[idx c] {:char      c
                               :note     (not (= c \.))
                               :value    (if (= c \.) 0 (xform c))
                               :dur      dur
                               :beat     (* idx dur)
                               })
                   (clojure.string/trim line))
      ;;(filter :note)
      (map (fn [itm]
             (if (:note itm)
               (ls/add-transform
                (ls/identity-item-transformer
                 (ls/notes-with-duration target (:dur itm) (:beat itm)))
                :dynamics
                (constantly (constantly (:value itm))))
               (ls/rest-with-duration (:dur itm) (:beat itm))
               )))
      (ls/concrete-logical-sequence)
      ))))



