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
    (->> (for [r this] (mapv f r)) vec))

  (esum [this]
    (if (vector? (first this))
      (reduce + (map #(reduce + %) this))
      (reduce + this)))

  (col-count [this]
    (count (first this)))

  (row-count [this]
    (count this))

  (m-update [this path f]
    (update-in this path f))

  (get-in-m [this path]
    (get-in this path))

  (creduce [this f]
    (reduce f (columns this)))

  (creduce [this f val]
    (reduce f val (columns this)))

  (rreduce [this f]
    (reduce f this))

  (rreduce [this f val]
    (reduce f val this))

  (cereduce [this f]
    (reduce f (map #(reduce f %) (columns this))))

  (cereduce [this f val]
    (reduce f val (map #(reduce f %) (columns this))))

  (rereduce [this f]
    (reduce f (map #(reduce f %) this)))

  (rereduce [this f val]
    (reduce f val (map #(reduce f %) this)))

  (col [this i]
    (mapv #(% i) this))

  (row [this i]
    (this i))

  (columns [this]
    (mapv #(col this %) (range (count (first this))))))

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


