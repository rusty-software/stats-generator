(ns stats-generator.main
  (:gen-class))

(def d6 [1 2 3 4 5 6])

(defn roll-d6 [_]
  (rand-nth d6))

(defn roll-4
  "Rolls a d6 4 times"
  []
  (map roll-d6 (range 4)))

(defn drop-lowest
  "Given a vector, returns a vector sans its lowest value."
  [v]
  (rest (sort v)))

(defn sum [v]
  (apply + v))

(defn roll-4-keep-3
  "Rolls a 6-sided die 4 times, summing the highest 3 values.

  Note that multiple arities exist, but the argument is ignored"
  ([_]
   (roll-4-keep-3))
  ([]
   (-> (roll-4)
       (drop-lowest)
       (sum))))

(defn roll-stats
  "Collects the results of 6 executions of rolling 4 dice and keeping the highest 3"
  []
  (map roll-4-keep-3 (range 6)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
