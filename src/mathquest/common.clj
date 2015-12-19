(ns mathquest.common)

(defn ^long square [^long x] (* x x))

(defn ^long cube [^long x] (* x x))

(defn ^double sqrt [^double x] (Math/sqrt x))

(defn ^double cbrt [^double x] (Math/cbrt x))

(defn ^double abs [x] (Math/abs x))
