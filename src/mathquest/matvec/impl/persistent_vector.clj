(ns mathquest.matvec.impl.persistent-vector
  (:require [mathquest.matvec.protocols :refer :all]))

(extend-type clojure.lang.PersistentVector
  IMatVec
  (transpose [this]
    (let [ctr (count (first this))]
      (loop [i (int 0) res (transient [])]
        (if (== i ctr)
          (persistent! res)
          (recur (inc i) (conj! res (mapv #(nth % i) this)))))))

  (cmap [this f]
    (let [ctr (count (first this))]
      (->> (for [i (range ctr)] (mapv #(nth % i) this)) (mapv f))))

  (rmap [this f]
    (mapv f this))

  (emap [this f]
    (->> (mapv f r) (for [r this]) vec)))
