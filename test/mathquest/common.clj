(ns mathquest.common
  (:require
    [clojure.test :refer :all]
    [mathquest.common :refer :all]))

(deftest common-tests
  (println "Testing common functions")
  (time
    (testing "gcd"
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
      (is (= 3 (apply gcd (take 100 (iterate #(+ 3 %) 3))))))))


