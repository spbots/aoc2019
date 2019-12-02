(require '[clojure.string :as str])

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(def input (map parse-int (str/split (slurp "./input/01") #"\n")))

(defn fuel-for [m]
    (if (> m 8)
        (- (Math/floor (/ m 3)) 2)
        0))

(defn fuel-for-2 [m]
    (def fuel-amount (- (Math/floor (/ m 3)) 2))
    (if (> m 8)
        (+ fuel-amount (fuel-for-2 fuel-amount))
        0))

(println (reduce (fn [acc x] (+ acc (fuel-for x))) 0 input))
(println (reduce (fn [acc x] (+ acc (fuel-for-2 x))) 0 input))
