(ns stats-generator.queens-test
  (:require [clojure.test :refer [deftest is]]
            [stats-generator.queens :as q]))

(deftest test-init-board
  (is (= {[0 0] false [0 1] false [0 2] false
          [1 0] false [1 1] false [1 2] false
          [2 0] false [2 1] false [2 2] false}
         (q/init-board 3))))

(deftest test-queen-in-row?
  (let [board {[0 0] false [0 1] false [0 2] false
               [1 0] false [1 1] false [1 2] true
               [2 0] false [2 1] false [2 2] false}]
    (is (not (q/queen-in-row? 0 board)))
    (is (q/queen-in-row? 1 board))))

(deftest test-queen-in-column?
  (let [board {[0 0] false [0 1] false [0 2] false
               [1 0] false [1 1] false [1 2] true
               [2 0] false [2 1] false [2 2] false}]
    (is (not (q/queen-in-column? 1 board)))
    (is (q/queen-in-column? 2 board))))

(deftest test-queen-in-diagonal?
  (let [board {[0 0] false [0 1] false [0 2] false
               [1 0] true [1 1] false [1 2] false
               [2 0] false [2 1] false [2 2] false}]
    (is (not (q/queen-in-diagonal? [0 2] board)))
    (is (q/queen-in-diagonal? [0 1] board))
    (is (q/queen-in-diagonal? [2 1] board))))

