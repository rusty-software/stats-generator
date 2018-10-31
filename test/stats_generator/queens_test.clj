(ns stats-generator.queens-test
  (:require [clojure.test :refer [deftest testing is]]
            [stats-generator.queens :as q]))

(deftest test-init-board
  (is (= {[0 0] false [0 1] false [0 2] false
          [1 0] false [1 1] false [1 2] false
          [2 0] false [2 1] false [2 2] false}
         (q/init-board 3))))

#_(deftest test-queen-space
  (let [board {[0 0] true [0 1] false [0 2] false
               [1 0] false [1 1] false [1 2] true
               [2 0] false [2 1] false [2 2] false}]
    (is (= [0 0] (q/queen-space board 0)))
    (is (= [1 2] (q/queen-space board 1)))
    (is (nil? (q/queen-space board 2)))))

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

(deftest test-queen-at-end-of-first-row?
  (is (q/queen-at-end-of-first-row? {[0 0] false [0 1] false [0 2] true
                                     [1 0] true [1 1] false [1 2] false
                                     [2 0] false [2 1] false [2 2] false}
                                    3))
  (is (not (q/queen-at-end-of-first-row? {[0 0] false [0 1] false [0 2] false
                                          [1 0] true [1 1] false [1 2] false
                                          [2 0] false [2 1] false [2 2] false}
                                         3))))

(deftest test-place-next-queen
  (testing "queen is placeable"
    (let [board {[0 0] true [0 1] false [0 2] false
                 [1 0] false [1 1] false [1 2] false
                 [2 0] false [2 1] false [2 2] false}
          {:keys [queen-placed? board]} (q/place-next-queen board 3 1)]
      (is queen-placed?)
      (is (= {[0 0] true [0 1] false [0 2] false
              [1 0] false [1 1] false [1 2] true
              [2 0] false [2 1] false [2 2] false}
             board))))
  (testing "queen is not placeable"
    (let [board {[0 0] true [0 1] false [0 2] false
                 [1 0] false [1 1] false [1 2] true
                 [2 0] false [2 1] false [2 2] false}
          {:keys [queen-placed? board]} (q/place-next-queen board 3 2)]
      (is (not queen-placed?))
      (is (= {[0 0] true [0 1] false [0 2] false
              [1 0] false [1 1] false [1 2] true
              [2 0] false [2 1] false [2 2] false}
             board)))))

(deftest test-move-placed-queen
  (testing "can move the queen"
    (let [board {[0 0] true [0 1] false [0 2] false [0 3] false
                 [1 0] false [1 1] false [1 2] true [1 3] false
                 [2 0] false [2 1] false [2 2] false [2 3] false
                 [3 0] false [3 1] false [3 2] false [3 3] false}
          {:keys [queen-moved? board]} (q/move-placed-queen board 4 1)]
      (is queen-moved?)
      (is (= {[0 0] true [0 1] false [0 2] false [0 3] false
              [1 0] false [1 1] false [1 2] false [1 3] true
              [2 0] false [2 1] false [2 2] false [2 3] false
              [3 0] false [3 1] false [3 2] false [3 3] false}
             board))))
  (testing "cannot move the queen"
    (let [board {[0 0] true [0 1] false [0 2] false [0 3] false
                 [1 0] false [1 1] false [1 2] false [1 3] true
                 [2 0] false [2 1] false [2 2] false [2 3] false
                 [3 0] false [3 1] false [3 2] false [3 3] false}
          {:keys [queen-moved? board]} (q/move-placed-queen board 4 1)]
      (is (not queen-moved?))
      (is (= {[0 0] true [0 1] false [0 2] false [0 3] false
              [1 0] false [1 1] false [1 2] false [1 3] false
              [2 0] false [2 1] false [2 2] false [2 3] false
              [3 0] false [3 1] false [3 2] false [3 3] false}
             board)))))

(deftest test-solve
  (testing "board is unsolvable"
    (let [state (q/solve 3)]
      (is (= :unsolvable (:result state)))))
  (testing "board is solvable"
    (let [state (q/solve 4)]
      (is (= :solved (:result state)))
      (is (= {[0 0] false [0 1] true [0 2] false [0 3] false
              [1 0] false [1 1] false [1 2] false [1 3] true
              [2 0] true [2 1] false [2 2] false [2 3] false
              [3 0] false [3 1] false [3 2] true [3 3] false}
             (:board state)))))
  )
