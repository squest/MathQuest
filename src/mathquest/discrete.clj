(ns mathquest.discrete
  (:require [mathquest.common :refer :all]))

#_(defn- ^long helper-gcd
    ([] 1)
    ([^long a] a)
    ([^long a ^long b]
     (cond (== 0 a) b
           (== 0 b) a
           (some #{1} [a b]) 1
           (> a b) (helper-gcd (- a b) b)
           :else (helper-gcd (- b a) a))))

(defn- ^long tail-gcd
  ([] 1)
  ([^long a] a)
  ([^long a ^long b]
   (loop [n (long a) m (long b)]
     (cond (== n 0) m
           (== m 0) n
           (some #{1} [n m]) 1
           (> n m) (recur (- n m) m)
           :else (recur n (- m n))))))

(defn gcd
  "Returns the greatest common divisors of one or more integers."
  [^longs & xs]
  (if (empty? xs)
    (tail-gcd)
    (reduce tail-gcd xs)))

(defn- helper-lcm
  ([] 1)
  ([a] (long a))
  ([a b]
   (let [g (gcd a b)]
     (* (quot a g) (quot b g) g))))

(defn lcm
  [^longs & xs]
  (if (empty? xs)
    (helper-lcm)
    (reduce helper-lcm xs)))

(defn ^longs divisors
  "Returns all the divisors of n"
  [^long n]
  (cond
    (== 1 n) [1]
    (even? n)
    (loop [i (int 2) res (transient [1 n])]
      (if (> (* i i) n)
        (-> res persistent! sort)
        (if (== (* i i) n)
          (-> (conj! res i) persistent! sort)
          (let [r (rem n i)]
            (if (== 0 r)
              (recur (+ 1 i) (conj! (conj! res i) (quot n i)))
              (recur (+ 1 i) res))))))
    (odd? n)
    (loop [i (int 3) res (transient [1 n])]
      (if (> (* i i) n)
        (-> res persistent! sort vec)
        (if (== (* i i) n)
          (-> (conj! res i) persistent! sort)
          (let [r (rem n i)]
            (if (== 0 r)
              (recur (+ 2 i) (conj! (conj! res i) (quot n i)))
              (recur (+ 2 i) res))))))))