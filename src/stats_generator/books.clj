(ns stats-generator.books
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defn ->books [raw-data]
  (map zipmap
       (->> (first raw-data)
            (map str/lower-case)
            (map #(str/replace % " " "-"))
            (map keyword)
            repeat)
       (rest raw-data)))

(defn most-frequent-authors [books]
  (let [author-book-counts (frequencies (map :author books))
        max-count (apply max (vec (vals author-book-counts)))]
    (filter #(= max-count (val %)) author-book-counts)))

(defn top-books-by-comparator [books comparator quantity]
  (let [page-counts (set (take quantity
                               (sort comparator (map (fn [book]
                                                        (-> (:number-of-pages book)
                                                            (Integer/parseInt)))
                                                      books))))]
    (filter #(page-counts (Integer/parseInt (:number-of-pages %))) books)))

(defn biggest-books [books]
  (top-books-by-comparator books > 3))

(defn smallest-books [books]
  (top-books-by-comparator books < 3))

(defn median-by-page-count [books]
  (let [books-count (count books)
        median-position (int (/ books-count 2))
        ordered-books (top-books-by-comparator books < books-count)]
    (last (take median-position ordered-books))))

(defn books-per-month [books]
  (let [dates (map :date-read books)
        months (map #(-> %
                         (clojure.string/split #"/")
                         (first)
                         (Integer/parseInt)) dates)
        month-counts (frequencies months)]
    (-> months
        (frequencies)
        (sort))))

(defn stats []
  (with-open [reader (io/reader "/Users/IronFuryMBP/Downloads/2017books.txt")]
    (let [raw-data (csv/read-csv reader :separator \tab)
          books (->books raw-data)
          mfa (filter #(<= 2 (second %)) (frequencies (map :author books)))
          total-pages (apply + (map (fn [s]
                                      (-> s
                                          :number-of-pages
                                          (Integer/parseInt)))
                                    books))
          bb (biggest-books books)
          sb (smallest-books books)
          page-count-mean (int (/ total-pages (count books)))
          page-count-median (median-by-page-count books)
          bpm (books-per-month books)
          media (frequencies (map :binding books))
          ]
      (println "Most frequent authors")
      (pprint mfa)
      (println)
      (println "Total pages:" total-pages)
      (println)
      (println "Biggest books")
      (pprint bb)
      (println)
      (println "Smallest books")
      (pprint sb)
      (println)
      (println "Mean page count:" page-count-mean)
      (println)
      (println "Median page count:" page-count-median)
      (println)
      (println "Books per month")
      (pprint bpm)
      (println)
      (println "Books per media")
      (pprint media)
      (println)
      ))
  )
