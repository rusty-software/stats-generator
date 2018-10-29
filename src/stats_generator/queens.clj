(ns stats-generator.queens)

(comment
  "The eight queens puzzle is the problem of placing eight chess queens on an 8×8 chessboard so that no two queens threaten each other. Thus, a solution requires that no two queens share the same row, column, or diagonal.")

(defn solve
  "Produces a solution to the queens puzzle given a number of queens. Board size will be the same dimension as number of queens."
  [n])

(defn init []
  (solve 8))
