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
  "Given a board and row, returns the column in which a queen resides, -1 if no queen is found."
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
  ([board board-size queen-to-place]
    (place-next-queen board board-size queen-to-place false))
  ([board board-size queen-to-place move-queen-right?]
   (loop [{:keys [board column] :as state} {:board board :column 0}]
     (let [queen-space-this-row (queen-space board queen-to-place)
           column (if (and move-queen-right? queen-space-this-row)
                    (inc (second queen-space-this-row))
                    column)
           space [queen-to-place column]]
       (cond
         (= board-size column)
         (assoc state :queen-placed? false)

         (not (or (queen-in-column? column board)
                  (queen-in-diagonal? space board)))
         (-> state
             (assoc-in [:board space] true)
             (assoc :queen-placed? true))

         :else
         (recur (update state :column inc)))))))

(defn solve
  "Produces a solution to the queens puzzle given a number of queens. Board size will be the same dimension as number of queens."
  [n]
  #_(let [board (init-board n)
        initial-state {:board board
                       :queen-to-place 0
                       :queen-placed? false}]
    (loop [{:keys [board queen-to-place queen-placed?] :as state} initial-state]
      (println board)
      (cond
        (= n queen-to-place)
        {:result :solved :board board}

        (and (not queen-placed?)
             (queen-at-end-of-first-row? board n))
        {:result :unsolvable :board board}

        (not queen-placed?)
        (let [placement-result (place-next-queen board n (dec queen-to-place) true)])

        :else
        (let [placement-result (place-next-queen board n queen-to-place)]
          (if (:queen-placed? placement-result)
            (recur (assoc state :queen-placed? true
                                :queen-to-place (inc queen-to-place)
                                :board (:board placement-result)))
            (recur (assoc state :queen-placed? false))))
        ))))

(defn init []
  (solve 8))
