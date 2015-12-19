(ns mathquest.test.discrete
  (:require
    [clojure.test :refer :all]
    [mathquest.common :refer :all]
    [mathquest.discrete :refer :all]
    [taoensso.timbre :as log]))

(set! *unchecked-math* true)

(deftest discrete-tests
  (log/info "Testing common functions")
  (time
    (testing "gcd"
      (log/info "GCD with single and multiple args")
      (is (= 1 (gcd)))
      (let [a (rand-int 1000)]
        (is (= a (gcd a)))
        (is (= a (gcd a a))))
      (let [a (rand-int 10000)
            b (rand-int 10000)]
        (is (= (gcd a b) (gcd a b a b a b)))
        (is (= (gcd a b) (gcd a a a a a b b b b b))))
      (is (= java.lang.Long (type (gcd 324 2344 234 2443 54353))))
      (is (= 1 (gcd 2 3 5 7 11)))
      (is (= 1 (gcd 11 13 17 19 23 29)))
      (is (= 3 (apply gcd (take 100 (iterate #(+ 3 %) 3)))))
      ;; gcd of a list of primes always 1
      (is (= 1 (apply gcd (sieve 10))))
      (is (= 1 (apply gcd (sieve 100))))
      (is (= 1 (apply gcd (sieve 1000))))))

  (time
    (testing "factorial"
      (log/info "Factorial")
      (is (= 1 (! 0)))
      (is (= 1 (! 1)))
      (is (= 2 (! 2)))
      (is (= (reduce * (range 1 10)) (! 9)))
      (is (= clojure.lang.BigInt (type (! 100))))
      (is (= java.lang.Long (type (! 12))))
      ;; Factorial results of n! must be divisible by all natural numbers
      ;; that are less than or equal n
      (is (= (repeat 10 true)
             (let [res (! 10)]
               (map #(zero? (rem res %)) (range 1 11)))))))

  (time
    (testing "divisors"
      (log/info "Divisors")
      (is (= [1 2] (divisors 2)))
      (is (= [1 3] (divisors 3)))
      (is (= clojure.lang.ArraySeq
             (type (divisors 100))))
      (is (= [1 2 3 6] (divisors 6)))
      (is (= [1 2 4] (divisors 4)))
      (is (= true (every? integer? (divisors 34353452342))))

      ;; Every element in divisors of n must be a divisor of n
      (is (= true
             (let [res (divisors 1234)]
               (every? #(zero? (rem 1234 %)) res))))
      (let [ndata 1000]
        (log/info "Testing divisors for" ndata " random integers")
        (is (= (repeat ndata true)
               (let [raws (repeatedly ndata #(rand-int 1000000))]
                 (for [r raws]
                   (let [res (divisors r)]
                     (every? #(zero? (rem r %)) res)))))))))

  (time
    (testing "lcm"
      (log/info "Testing LCM with various arities")
      (is (= 1 (lcm)))
      (let [res (repeatedly 10 #(rand-int 1000))]
        (is (= res (map lcm res)))
        (is (= res (map lcm res res)))
        (is (= res (map lcm res res res)))
        (is (= (repeat 10 java.lang.Long)
               (map type (map lcm res res res res res)))))
      (let [tmp (filter prime? (range 10 30))]
        (is (= (apply * tmp) (apply lcm tmp)))
        (let [tmp2 (shuffle tmp) f #(take % tmp2)]
          (is (= (apply * (f 2)) (apply lcm (f 2))))
          (is (= (apply * (f 3)) (apply lcm (f 3))))
          (is (= (apply * (f 4)) (apply lcm (f 4))))
          (is (= (apply * (f 5))
                 (apply lcm (take 10 (cycle (f 5))))))
          (is (= (apply * (f 5))
                 (apply lcm (take 20 (cycle (f 5))))))))

      ;; Every input of lcm must divides the lcm result
      (let [ninput 10
            raw (repeatedly ninput #(rand-int 1000))
            result (apply lcm raw)]
        (log/info "Testing lcm for" ninput "random integers")
        (is (= (repeat 10 true)
               (map #(zero? (rem result %)) raw)))))))

(deftest primality-tests
  (log/info "Testing all prime related functions")
  (time
    (testing "Trial division primality check"
      ;; all integers less than 1 are not positive primes
      (is (= (repeat 10 false)
             (map prime? (take 10 (iterate dec 1)))))
      (is (= [2 3 5 7 11 13 17 19]
             (filter prime? (range 20))))
      ;; it can handle big numbers too
      (is (= (repeat 2 true) (map prime? [2147483647 67280421310721])))
      ;; fast enough to check 2000000 even numbers
      ;; Should be done in less than 500ms
      (let [timeout 400 maxi 4000000]
        (is (= true
               (let [job (->> (range 4 maxi 2)
                              (every? #(false? (prime? %)))
                              future)]
                 (do (Thread/sleep timeout)
                     (future-done? job))))))))

  (time
    (testing "Prime generator and sum of primes"
      (is (= [2 3 5 7 11 13 17 19] (sieve 20)))
      (is (= (reduce + [2 3 5 7 11 13 17 19])
             (sum-sieve 20)))
      (is (= (reduce + (sieve 1000))
             (sum-sieve 1000)))
      (is (= (filter prime? (range 1000))
             (sieve 1000)))
      (is (= true (every? #(< % 1000) (sieve 1000))))

      ;; Every prime number must not have a divisor other than itself
      (is (= true
             (->> (for [i (sieve 1000)]
                    (->> (range 3 i)
                         (every? #(pos? (rem i %)))))
                  (every? true?))))
      ;; other than 2, the other positive primes must be odd
      (is (= true (->> (sieve 1000) rest (every? odd?))))))

  (time
    (testing "Prime factors calculation"
      (is (= clojure.lang.PersistentArrayMap
             (type (prime-factors 100))))
      ;; Every even numbers must have 2 as one of its factor
      (is (= (repeat 20 2)
             (->> (range 10 50 2)
                  (map prime-factors)
                  (map keys)
                  (map #(some #{2} %)))))

      (is (= {2 2 3 2 5 2 7 2}
             (prime-factors (* 2 2 3 3 5 5 7 7))))

      ;; You can always get the result back by doing the inverse process
      (let [a (rand-int 1000)
            ares (prime-factors a)]
        (is (= a (reduce * (map #(let [[a b] %] (expt a b)) ares)))))

      (is (= (repeat 100 true)
             (for [i (repeatedly 100 #(rand-int 10000))]
               (let [ares (prime-factors i)]
                 (== i (reduce * (map #(let [[a b] %] (expt a b)) ares))))))))))

(deftest exponential-tests
  (log/info "Testing exponential functions")
  (time
    (testing "Basic exponential function"
      (let [a (rand-int 1000)]
        (is (= (apply *' (repeat 10 a))
               (expt a 10))))
      (let [i-raws (repeatedly 100 #(rand-int 1000))
            m (rand-int 10)]
        (is (= (repeat 100 true)
               (-> #(== (expt % m)
                        (reduce *' (repeat m %)))
                   (map i-raws)))))))
  (time
    (testing "Modular exponential function"
      (let [i-raws (repeatedly 100 #(rand-int 100))
            j-raws (->> (repeatedly #(rand-int 100))
                        (filter pos?)
                        (take 100))
            m (loop []
                (let [res (rand-int 5)]
                  (if (some #{res} [0 1]) (recur) res)))]
        (is (= (repeat 100 true)
               (-> #(== (rem (expt % m) %2)
                        (mod-expt % m %2))
                   (map i-raws j-raws))))
        (is (= (repeat 100 true)
               (-> #(== (rem (bigint (Math/pow % m)) %2)
                        (mod-expt % m %2))
                   (map i-raws j-raws))))))))

(deftest coprimality-tests
  (log/info "Testing coprimality functions and Euler's totient")
  (time
    (testing "Coprimality"
      (is (= true (all-coprime? 2 3 5 7 11 13 17 19)))
      (is (= true (coprime? 2 3 5 7 11 13 17 19)))
      (is (= false (coprime? 11 13 17 19 (* 2 11))))
      (is (= [] (filterv (partial coprime? 2) (range 4 20 2))))
      (is (= (filterv #(pos? (rem % 3)) (range 2 20))
             (filterv (partial coprime? 3) (range 2 20))))
      (let [a (rand-int 1000)]
        (is (= (filterv #(== 1 (gcd a %)) (range 1 a))
               (coprimes a)))))))


