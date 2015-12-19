(ns mathquest.matvec
  (:require
    [mathquest.matvec.protocols :as p]))

(defn transpose
  "Transpose a matrix"
  [mat]
  (p/transpose mat))

(defn cmap
  "Column-wise map function"
  [f mat]
  (p/cmap mat f))

(defn rmap
  "Row-wise map function"
  [f mat]
  (p/rmap mat f))

(defn emap
  "Element-wise map function"
  [f mat]
  (p/emap mat f))

(defn creduce
  "Column-wise reduce function with i as initial value"
  ([f mat] (creduce mat f))
  ([f i mat] (creduce mat f i)))

(defn rreduce
  "Column-wise reduce function with i as initial value"
  ([f mat] (rreduce mat f))
  ([f i mat] (rreduce mat f i)))

(defn cereduce
  "Column-wise reduce function with i as initial value"
  ([f mat] (cereduce mat f))
  ([f i mat] (cereduce mat f i)))

(defn rereduce
  "Column-wise reduce function with i as initial value"
  ([f mat] (rereduce mat f))
  ([f i mat] (rereduce mat f i)))


