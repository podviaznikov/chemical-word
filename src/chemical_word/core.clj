(ns chemical-word.core)


(def periodic-table
  {1 "H"
   2 "He"
   3 "Li"
   4 "Be"
   5 "B"
   6 "C"
   7 "N"
   8 "O"
   9 "F"
   10 "Ne"})


(def all-words (clojure.string/split (slurp "/usr/share/dict/words") #"\n"))


(def words
  (take 20000
    (filter #(> (count %) 4) all-words))
  )



; steps
; cleanup words from dictionary less than 5 characters -> it's not fun to have short words.
;think about removing names of people from the words
; 1. take first element
; 2. find if some word start with it

; BaCoN

; find all words that start with Ba
; add Co and find all words that starts with BaCo
; add N and return BaCoN since there is a complete match!



(defn words-starting-with [predicate words]
  (filter #(.startsWith (.toLowerCase %) (.toLowerCase predicate)) words))

(defn match-words [predicate words]
  (filter #(= (.toLowerCase %) (.toLowerCase predicate)) words))


(defn matched-words [predicate words]
  (let [filtered (match-words (:term predicate) words)]
    (if (> (count filtered) 0)
      [{:id (:term predicate)
        :elements (:elements predicate)
        :words filtered}]
      )))


(defn new-predicate [element]
  (let [predicate (assoc {} :elements [element]
                            :term (val element))]
    predicate))

(defn update-predicate [predicate element]
  (let [new-term (str (:term predicate) (val element))
        elements (conj (:elements predicate) element)
        new-predicate (assoc predicate :elements elements
                                   :term new-term)]
    new-predicate))

(defn search-iter [predicate words final-words]
  (let [filtered-words (words-starting-with (:term predicate) words)
        matched (matched-words predicate words)
        final-words (conj final-words matched)]
        (if (= (count filtered-words) 0)
          final-words
          (for [el periodic-table]
            (let [new-predicate (update-predicate predicate el)]
              (search-iter new-predicate filtered-words final-words)))
          )))

;(flatten (search-iter "Beli" words '()))


(defn search [words]
  (apply conj #{}

    (filter identity
      (flatten
        (for [element periodic-table]
          (let [predicate (new-predicate element)]
            (search-iter predicate words nil))))
      )))
;(words-starting-with "BeLi" words)

;(matched-words "BeLiLi" words)

;(filter identity (search words))

;(count (search words))









