(ns mathquest.matvec
  (:require
    [mathquest.matvec.protocols :as p]
    [clojure.core.matrix :as mat]))

(mat/set-current-implementation :vectorz)

;; Immutable Sequence like operations to matrix and/or vector

(defn cmap
  "Column-wise map function"
  [f m]
  (p/cmap m f))

(defn rmap
  "Row-wise map function"
  [f m]
  (p/rmap m f))

(defn emap
  "Element-wise map function"
  [f m]
  (p/emap m f))

(defn columns
  "Returns the sequence of columns of a matrix"
  [m]
  (p/columns m))

(defn col-count
  "Returns the number of columns of a matrix"
  [m]
  (p/col-count m))

(defn row-count
  "Returns the number of rows of a matrix"
  [m]
  (p/row-count m))

(defn col
  "Returns the sequence of value of a matrix at i-th column (index start from 0)"
  [m i]
  (p/col m i))

(defn row
  "Returns the i-th row of a matrix (idx starts from 0)"
  [m i]
  (p/row m i))

(defn m-update
  [m path f]
  (p/m-update m path f))

(defn get-in-m
  [m path]
  (p/get-in-m m path))

(defn esum
  "Element-wise summation of a matrix/vector"
  [m]
  (p/esum m))

(defn creduce
  "Column-wise reduce function with i as initial value"
  ([f m] (p/creduce m f))
  ([f i m] (p/creduce m f i)))

(defn rreduce
  "Column-wise reduce function with i as initial value"
  ([f m] (p/rreduce m f))
  ([f i m] (p/rreduce m f i)))

(defn cereduce
  "Column-wise reduce function with i as initial value"
  ([f m] (p/cereduce m f))
  ([f i m] (p/cereduce m f i)))

(defn rereduce
  "Column-wise reduce function with i as initial value"
  ([f m] (p/rereduce m f))
  ([f i m] (p/rereduce m f i)))

;; Mutable variant of the above

(defn cmap!
  "Mutable variant of cmap"
  [f m]
  (p/cmap! m f))

(defn rmap!
  "Mutable variant of rmap"
  [f m]
  (p/rmap! m f))

(defn emap!
  "Mutable variant of emap"
  [f m]
  (p/emap! m f))

(defn m-update!
  "Mutable variant of m-update"
  [m path f]
  (p/m-update! m path f))

;; Vector operations

(defn dot
  "Returns the dot product of two vectors u and v"
  [u v]
  (p/dot u v))

(defn cross
  "Returns the cross product of two vectors u and v"
  [u v]
  (p/cross u v))

(defn project
  "Returns the projection of u to v. The result is a map consisted of both
  the scalar value and the vector."
  [u v]
  (p/project u v))

(defn magnitude
  "Returns the magnitude of a vector v"
  [v]
  (p/magnitude v))

(defn smul
  "Returns the scalar multiplication of v with a scalar k"
  [k v]
  (p/smul v k))

;; Matrix operations

(defn transpose
  "Transpose a matrix"
  [m]
  (p/transpose m))

(defn det
  "Returns the determinant of a matrix"
  [m]
  (p/det m))

(defn inverse
  "Returns the inverse of the matrix assuming the matrix is inversible"
  [m]
  (p/inverse m))

(defn mmul
  "Returns the multiplication of two matrix, the dimensions must be compatible"
  [A B]
  (p/mmul A B))

(defn madd
  "Returns the addition of two matrix A & B"
  [A B]
  (p/madd A B))




















