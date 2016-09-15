(ns composition-kit.physical-to-logical
  (:require [composition-kit.midi-util :as midi])
  (:require [composition-kit.physical-sequence :as ps])
  )

(defn schedule-logical-on-physical
  [in-seq pattern instrument clock]
  ;; This is basically a massive reduce statement
  (reduce (fn [pseq item]
            pseq
            )
          in-seq
          pattern)
  )
