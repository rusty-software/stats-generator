(ns stats-generator.queens)

(comment
  "The eight queens puzzle is the problem of placing eight chess queens on an 8×8 chessboard so that no two queens threaten each other. Thus, a solution requires that no two queens share the same row, column, or diagonal.")

(defn init-board
  "Returns a map of spaces and booleans for a square of n size."
  [n]
  (into {}
        (for [r (range n)
              c (range n)]
          [[r c] false])))

(defn queen-space
  "Given a board and row, returns the coordinates in which a queen resides, nil if no queen is found."
  [board row-num]
  (let [space (filter #(and (= row-num (first (key %)))
                            (val %))
                      board)]
    (if (seq space)
      (key (first space))
      nil)))

(defn queen-in-row?
  "Given a row number and a board, returns true if a queen is in the row, otherwise false."
  [row-num board]
  (boolean (first (filter #(and (= row-num (first (key %)))
                                (val %))
                          board))))

(defn queen-in-column?
  "Given a column number and a board, returns true if a queen is in the column, otherwise false."
  [col-num board]
  (boolean (first (filter #(and (= col-num (second (key %)))
                                (val %))
                          board))))

(defn abs [n]
  (if (neg? n) (* -1 n) n))

(defn queen-in-diagonal?
  "Given a space and a board, returns true if a queen is in a diagonal, otherwise false."
  [[row column] board]
  (boolean (first (filter #(and (= (abs (- row (first (key %))))
                                   (abs (- column (second (key %)))))
                                (val %))
                          board))))

(defn queen-at-end-of-first-row?
  "Given a board, returns true if the queen in row 0 in in the space furthest right."
  [board board-size]
  (boolean (first (filter #(and (zero? (first (key %)))
                                (= (dec board-size) (second (key %)))
                                (val %))
                          board))))

(defn place-next-queen
  "Given a board, size, and a queen number to place, returns a placement result with an indicator of whether or not the queen was successfully placed and an updated board.

  Uses the fact that only one queen per row is allowed, and only attempts insertion into that row."
  [board board-size queen-to-place]
  (loop [{:keys [board column] :as state} {:board board :column 0}]
     (let [space [queen-to-place column]]
       (cond
         (= board-size column)
         (assoc state :queen-placed? false)

         (not (or (queen-in-column? column board)
                  (queen-in-diagonal? space board)))
         (-> state
             (assoc-in [:board space] true)
             (assoc :queen-placed? true))

         :else
         (recur (update state :column inc))))))

(defn move-placed-queen
  "Given a board, board size, and row number, returns a placement result with an indicator of whether or not the queen was successfully moved and an updated board."
  [board board-size row-num]
  (let [queen-loc (queen-space board row-num)
        column-num (second queen-loc)
        updated-board (assoc board queen-loc false)
        initial-state {:board updated-board
                       :column (inc column-num)}]
    (loop [{:keys [board column] :as state} initial-state]
      (let [space [row-num column]]
        (cond
          (= board-size column)
          (assoc state :queen-moved? false)

          (not (or (queen-in-column? column board)
                   (queen-in-diagonal? space board)))
          (-> state
              (assoc-in [:board space] true)
              (assoc :queen-moved? true))

          :else
          (recur (update state :column inc)))))))

(defn solve
  "Produces a solution to the queens puzzle given a number of queens. Board size will be the same dimension as number of queens."
  [n]
  (let [board (init-board n)
        initial-state {:board board
                       :queen-to-place 0
                       :queen-placed? true}]
    (loop [{:keys [board queen-to-place queen-placed?] :as state} initial-state]
      (cond
        (= n queen-to-place)
        {:result :solved :board board}

        (and (not queen-placed?)
             (queen-at-end-of-first-row? board n))
        {:result :unsolvable :board board}

        (not queen-placed?)
        (let [placement-result (move-placed-queen board n (dec queen-to-place))]
          (if (:queen-moved? placement-result)
            (recur (assoc state :queen-placed? true
                                :queen-to-place queen-to-place
                                :board (:board placement-result)))
            (recur (assoc state :queen-placed? false
                                :queen-to-place (dec queen-to-place)
                                :board (:board placement-result)))))

        :else
        (let [placement-result (place-next-queen board n queen-to-place)]
          (if (:queen-placed? placement-result)
            (recur (assoc state :queen-placed? true
                                :queen-to-place (inc queen-to-place)
                                :board (:board placement-result)))
            (recur (assoc state :queen-placed? false))))))))

(defn print-board [board board-size]
  (let [board (into {} (sort board))]
    (doseq [row (range board-size)]
      (loop [column 0
             output ""]
        (if (= board-size column)
          (println output)
          (recur
            (inc column)
            (str output
                 (if (get board [row column])
                   "X|"
                   "_|"))))))))

(defn init []
  (let [{:keys [result board]} (solve 8)]
    (println "RESULT:" result)
    (print-board board 8)))
