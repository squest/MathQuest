(ns mathquest.matvec.protocols)

(defprotocol IMVSequence
  (cmap [this f])
  (rmap [this f])
  (emap [this f])
  (esum [this])
  (columns [this])
  (row-count [this])
  (col-count [this])
  (col [this i])
  (row [this i])
  (get-in-m [this path])
  (m-update [this path f])
  (creduce [this f] [this f val])
  (rreduce [this f] [this f val])
  (cereduce [this f] [this f val])
  (rereduce [this f] [this f val]))

(defprotocol IMVMutable
  (cmap! [this f])
  (rmap! [this f])
  (emap! [this f])
  (m-update! [this path f]))

(defprotocol IVectorOperations
  (dot [this v])
  (cross [this v])
  (smul [this k])
  (project [this v])
  (magnitude [this]))

(defprotocol IMatrixOperations
  (mmul [this v])
  (madd [this v])
  (inverse [this])
  (transpose [this])
  (det [this]))


