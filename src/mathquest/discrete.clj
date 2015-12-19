(ns mathquest.discrete
  (:require
    [mathquest.common :refer :all]
    [clojure.set :as cs]))

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

(defn ^boolean prime?
  "Naive trial division prime checking."
  [^long n]
  (cond (< n 2) false
        (== n 2) true
        (even? n) false
        :else (let [lim (-> n sqrt int inc)]
                (loop [i (int 3)]
                  (cond
                    (> i lim) true
                    (== 0 (rem n i)) false
                    :else (recur (+ i 2)))))))

(defn !
  "Returns the n factorial"
  [^long n]
  (reduce *' (range 1 (inc n))))

(defn fact-mod
  "Returns the n factorial mod m"
  [^long n ^long m]
  (reduce #(rem (*' % %2) m) (range 1 (inc n))))

(defn expt
  "Returns a^m"
  [^long a ^long m]
  (cond (== m 0) 1
        (== m 1) a
        :else (let [res (expt a (quot m 2))]
                (*' res res (if (even? m) 1 a)))))

(defn ^long mod-expt
  "Returns modular exponent of a^m mod n."
  [^long a ^long m ^long n]
  (cond (== m 0) 1
        (== m 1) (rem a n)
        :else (let [res (mod-expt a (quot m 2) n)]
                (rem (*' res res (if (even? m) 1 a)) n))))

(defn ^long gcd
  "Returns the greatest common divisors of one or more integers."
  [ & xs]
  (if (empty? xs)
    (tail-gcd)
    (reduce tail-gcd xs)))

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

(defn prime-factors
  "Returns the prime factors of n, map version"
  [n]
  (let [lim (-> n sqrt long inc)]
    (loop [i (int 2) res {} cur n]
      (cond (or (> i lim) (> i cur))
            (if (prime? cur) (merge res {cur 1}) res)
            (== 0 (rem cur i))
            (if (prime? i)
              (let [kom (iterate #(quot % i) cur)
                    tmp (->> kom
                             (drop-while #(== 0 (rem % i)))
                             first)
                    ctr (->> kom
                             (take-while #(== 0 (rem % i)))
                             count)]
                (recur (+ i 1) (merge res {i ctr}) tmp))
              (recur (+ i 1) res cur))
            :else (recur (+ i 1) res cur)))))

(defn- helper-lcm
  ([] 1)
  ([a] a)
  ([a b]
   (let [divs-a (prime-factors a)
         divs-b (prime-factors b)]
     (->> (merge-with max divs-a divs-b)
          (reduce #(*' % (expt (key %2) (val %2))) 1)))))

(defn ^long lcm
  "Returns the least common multiple of one integer or more."
  [& xs]
  (if (empty? xs)
    (helper-lcm)
    (reduce helper-lcm xs)))

(defn ^longs sieve
  "Returns positive primes not greater than lim"
  [^long lim]
  (let [llim (-> lim sqrt long inc)
        refs (boolean-array (inc lim) true)]
    (loop [i (int 3) res (transient [2])]
      (cond (>= i lim)
            (persistent! res)
            (<= i llim)
            (if (aget refs i)
              (do (loop [j (int (* i i))]
                    (when (<= j lim)
                      (aset refs j false)
                      (recur (+ j i i))))
                  (recur (+ i 2) (conj! res i)))
              (recur (+ i 2) res))
            :else
            (if (aget refs i)
              (recur (+ i 2) (conj! res i))
              (recur (+ i 2) res))))))

(defn ^long sum-sieve
  "Returns the sum of positive primes not greater than lim"
  [^long lim]
  (let [llim (-> lim sqrt int inc)
        refs (boolean-array (inc lim) true)]
    (loop [i (int 3) res (int 2)]
      (cond (>= i lim)
            res
            (<= i llim)
            (if (aget refs i)
              (do (loop [j (int (* i i))]
                    (when (<= j lim)
                      (aset refs j false)
                      (recur (+ j i i))))
                  (recur (+ i 2) (+ res i)))
              (recur (+ i 2) res))
            :else
            (if (aget refs i)
              (recur (+ i 2) (+ res i))
              (recur (+ i 2) res))))))

;; useless stuffs but probably important to keep

(comment
  ;; We have a faster lcm for large integers
  (defn- helper-lcm-x
    ([] 1)
    ([a] (long a))
    ([a b]
     (let [g (gcd a b)]
       (* (quot a g) (quot b g) g))))

  (defn lcm-x
    [& xs]
    (if (empty? xs)
      (helper-lcm-x)
      (reduce helper-lcm-x xs)))

  (defn- ^long helper-gcd
    ([] 1)
    ([^long a] a)
    ([^long a ^long b]
     (cond (== 0 a) b
           (== 0 b) a
           (some #{1} [a b]) 1
           (> a b) (helper-gcd (- a b) b)
           :else (helper-gcd (- b a) a)))))