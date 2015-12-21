(ns mathquest.matvec.impl.persistent-vector
  (:require
    [mathquest.matvec.protocols :refer :all]
    [mathquest.common :refer :all]))

(extend-type clojure.lang.PersistentVector

  IMVSequence

  (cmap [this f]
    (let [ctr (count (first this))]
      (->> (for [i (range ctr)] (mapv #(nth % i) this)) (mapv f))))

  (rmap [this f]
    (mapv f this))

  (emap [this f]
    (->> (mapv f r) (for [r this]) vec)))

(extend-type clojure.lang.PersistentVector

  IVectorOperations

  (smul [this k]
    (mapv (partial * k) this))

  (dot [this v]
    (->> (map #(* %1 %2) this v)
         (reduce +)))

  (magnitude [this]
    (->> (map square this) (reduce +) sqrt))

  (project [this v]
    (let [magb (magnitude v)
          adotb (dot this v)
          sres (/ adotb magb 1.0)]
      {:scalar sres
       :vector (smul v (/ sres magb))})))

(extend-type clojure.lang.PersistentVector

  IMatrixOperations

  (transpose [this]
    (let [ctr (count (first this))]
      (loop [i (int 0) res (transient [])]
        (if (== i ctr)
          (persistent! res)
          (recur (inc i) (conj! res (mapv #(nth % i) this))))))))


