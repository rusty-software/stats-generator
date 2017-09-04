(ns stats-generator.main-test
  (:require [clojure.test :refer :all]
            [stats-generator.main :as main]))

(deftest test-roll-4
  (let [roll (main/roll-4)]
    (is (= 4 (count roll)))
    (is (every? #(<= 1 % 6) roll))))

(deftest test-drop-lowest
  (testing "Given all different values in random order, drops the lowest"
    (is (= [2 3 4] (main/drop-lowest [1 2 3 4])))
    (is (= [2 3 4] (main/drop-lowest [4 2 3 1]))))
  (testing "Given identical values, drops one of them"
    (is (= [1 1 1] (main/drop-lowest [1 1 1 1]))))
  (testing "Given some duplicated values, drops whatever's lowest"
    (is (= [2 2 3] (main/drop-lowest [2 3 2 2])))
    (is (= [5 6 6] (main/drop-lowest [5 6 4 6])))))

(deftest test-sum
  (is (= 18 (main/sum [6 6 6])))
  (is (= 3 (main/sum [1 1 1])))
  (is (= 12 (main/sum [3 4 5]))))

(deftest test-roll-4-keep-3
  (testing "Given a roll of 4 6-sided dice, sums the highest 3"
    (is (<= 3 (main/roll-4-keep-3) 18))))

(deftest test-roll-stats
  (let [stats (main/roll-stats)]
    (is (= 6 (count stats)))
    (is (every? #(<= 3 % 18) stats))))
