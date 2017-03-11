(ns phone-number)

(defn fewer-than-10-digits? [n]
  (< (count n) 10)
  )

(defn exactly-11-digits? [n]
  (= (count n) 11)
  )

(defn drop-first-get-rest [n]
  (.substring n 1)
  )

(defn starts-with-1? [n]
  (= "1" (.substring n 0 1))
  )

(defn extract-digits [n]
  (apply str (filter #(. Character (isDigit %)) n))
  )

(def bad-number "0000000000")

(defn number [number]
  (let [n (extract-digits number)]
    (if (fewer-than-10-digits? n)
      bad-number
      (if (exactly-11-digits? n)
        (if (starts-with-1? n)
          (drop-first-get-rest n)
          bad-number
          )
        n
        )
      )
    )
  )

(defn area-code [number]
  (let [n (extract-digits number)]
    (if (fewer-than-10-digits? n)
      bad-number
      (if (exactly-11-digits? n)
        (.substring (drop-first-get-rest n) 0 3)
        (.substring n 0 3)
        )
      )
    )
  )

(defn first-3 [n]
  (.substring n 3 6)
  )

(defn tail [n]
  (.substring n 6)
  )

(defn pp [n]
  (str "(" (area-code n) ") " (first-3 n) "-" (tail n))
  )

(defn pretty-print [number]
  (let [n (extract-digits number)]
    (if (fewer-than-10-digits? n)
      bad-number
      (if (exactly-11-digits? n)
        (pp (drop-first-get-rest n))
        (pp n)
        )
    )
  )
  )



