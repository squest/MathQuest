(ns mathquest.test.discrete
  (:require
    [clojure.test :refer :all]
    [mathquest.common :refer :all]
    [mathquest.discrete :refer :all]
    [taoensso.timbre :as log]))

(deftest discrete-tests
  (println "Testing common functions")
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
      (is (= 3 (apply gcd (take 100 (iterate #(+ 3 %) 3)))))))

  (time
    (testing "divisors"
      (log/info "Divisors")
      (is (= [1 2] (divisors 2)))
      (is (= [1 3] (divisors 3)))
      (is (= clojure.lang.ArraySeq
             (type (divisors 100))))
      (is (= [1 2 3 6] (divisors 6)))
      (is (= [1 2 4] (divisors 4)))
      (is (= true (every? integer? (divisors 34353452342))))))

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
                 (apply lcm (take 20 (cycle (f 5)))))))))))


