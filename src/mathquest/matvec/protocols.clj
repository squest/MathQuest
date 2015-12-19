(ns mathquest.matvec.protocols)

(defprotocol IMatVec
  (transpose [this]
    "Transpose a matrix or a vector.")
  (cmap [this f]
    "Column-wise map function")
  (rmap [this f]
    "Row-wise map function")
  (emap [this f]
    "Element-wise map function")
  (creduce [this f] [this f val]
    "Column-wise reduce function with val as initial value")
  (rreduce [this f] [this f val]
    "Row-wise reduce function with val as initial value")
  (cereduce [this f] [this f val]
    "Element-wise reduce function by column with val as initial value")
  (rereduce [this f] [this f val]
    "Element-wise reduce function by row with val as initial value"))


