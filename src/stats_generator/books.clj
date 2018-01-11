(ns stats-generator.books
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io]
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

(defn stats []
  (with-open [reader (io/reader "/Users/IronFuryMBP/Downloads/2017books.txt")]
    (let [raw-data (csv/read-csv reader :separator \tab)
          books (->books raw-data)
          mfa (most-frequent-authors books)
          total-pages (apply + (map (fn [s]
                                      (-> s
                                          :number-of-pages
                                          (Integer/parseInt)))
                                    books))]
      ))

  )
