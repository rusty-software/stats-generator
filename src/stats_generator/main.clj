(ns stats-generator.main
  (:gen-class))

(def d6 [1 2 3 4 5 6])

(defn ^:private roll-d6 []
  (rand-nth d6))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
