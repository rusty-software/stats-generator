(ns stats-generator.main
  (:gen-class)
  (:import [java.util Date]))

(def d6 [1 2 3 4 5 6])
(def range-3-18 (range 3 19))

(defn roll-d6 [_]
  (rand-nth d6))

(defn roll-d3-18 [_]
  (rand-nth range-3-18))

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

(defmulti roll-stats identity)

(defmethod roll-stats :roll-4-keep-3
  [_]
  (map roll-4-keep-3 (range 6)))

(defmethod roll-stats :range-3-to-18
  [_]
  (map roll-d3-18 (range 6)))

(defn now [] (new java.util.Date))

(defn roll-all-18s []
  (println "STARTED AT:" (now))
  (loop [counter 1
         current-roll (roll-stats)]
    (cond
      (= [18 18 18 18 18 18] current-roll)
      (do
        (println "All 18s took" counter "attempt(s).")
        (println "ENDED AT:" (now)))

      (= 100000000 counter)
      (do
        (println "Gave up after ONE HUNDRED MILLION sets...")
        (println "ENDED AT:" (now)))

      :default
      (do
        (when (<= 5 (count (filter #(= 18 %) current-roll)))
          (println "close call!" current-roll))
        (recur (inc counter)
               (roll-stats))))))

(defn -main
  [& args]
  (roll-all-18s :range-3-to-18))
