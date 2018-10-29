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

(defn queen-in-row?
  "Given a row number and a board, returns true if a queen is in the row, otherwise false."
  [row-num board]
  (boolean (first (filter #(and (= row-num (first (key %)))
                                (val %)) board))))

(defn queen-in-column?
  "Given a column number and a board, returns true if a queen is in the column, otherwise false."
  [col-num board]
  (boolean (first (filter #(and (= col-num (second (key %)))
                                (val %)) board))))

(defn abs [n]
  (if (neg? n) (* -1 n) n))

(defn queen-in-diagonal?
  "Given a space and a board, returns true if a queen is in a diagonal, otherwise false."
  [[row column] board]
  (boolean (first (filter #(and (= (abs (- row (first (key %))))
                                   (abs (- column (second (key %)))))
                                (val %)) board)))
  #_(let [board-size (inc (apply max (map first (keys board))))
        diagonal-rows (concat (range row) (range row board-size))
        diagonal-columns (concat (range column) (range column board-size))]
    (println diagonal-rows)
    (println diagonal-columns)))

(defn solve
  "Produces a solution to the queens puzzle given a number of queens. Board size will be the same dimension as number of queens."
  [n])

(defn init []
  (solve 8))
