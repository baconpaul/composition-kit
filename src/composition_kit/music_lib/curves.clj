(ns composition-kit.music-lib.curves)

;; A library of various curve implementations so I can stop coding them up all over the place. Each of these is a function
;; from a set of parameters to a single variable function

(defn line [x0 y0 x1 y1]
  "Returns a function x->y with constant slope and f(x0) = y0 f(x1) = y1"
  (let [dx (- x1 x0)
        dy (- y1 y0)
        slope (/ dy dx)
        ]
    (fn [x] (+ y0 (* (- x x0) slope)))
    )
  )

(defn sigmoid
  "A sigmoid (or s-shaped function) 1/(1+e^-x) shifted, which asymptotes between y0 and y1 with the curve 'between' x0 and x1"
  ([x0 y0 x1 y1] (sigmoid x0 y0 x1 y1 4))
  ([x0 y0 x1 y1 s]
   (let [x-shift (line x0 (- s) x1 s)
         r-shift (line 0 y0 1 y1)
         ]
     (fn [x]
       (let [t (x-shift x)
             r (/ 1 (inc (Math/exp (- t))))
             ]
         (r-shift r))
       )
     )
   )
  )


