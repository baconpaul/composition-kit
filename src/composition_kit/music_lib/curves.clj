(ns composition-kit.music-lib.curves)

;; A library of various curve implementations so I can stop coding them up all over the place. Each of these is a function
;; from a set of parameters to a single variable function

(defn line [x0 y0 x1 y1]
  (let [dx (- x1 x0)
        dy (- y1 y0)
        slope (/ dy dx)
        ]
    (fn [x] (+ y0 (* (- x x0) slope)))
    )
  )

(defn sigmoid
  ([x0 y0 x1 y1] (sigmoid x0 y0 x1 y1 4))
  ([x0 y0 x1 y1 s]
   (let [x-shift (line x0 (- s) x1 s)
         r-shift (line 0 y0 1 y1)
         ]
     (fn [x]
       (let [t (x-shift x)
             r (/ 1 (+ 1 (Math/exp (- t))))
             ]
         (r-shift r))
       )
     )
   )
  )

(let [q (sigmoid 1 1 8 7 4)]
  (map q (range 10)))

