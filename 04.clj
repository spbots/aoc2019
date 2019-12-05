(require '[clojure.string :as str])

; number to digits or digits to number (then check range)
(def MINVAL 123257)
(def MAXVAL 647015)

; https://stackoverflow.com/a/22772070/101645
(defn digits [x]
    (if (< x 10)
        [x]
        (conj (digits (quot x 10)) (rem x 10))))

(defn increasing-digits? [x]
    (>= (reduce (fn [a b] (if (and
        (>= b a) (>= a 0)) b -1)) 0 x) 0))

(defn collapse-digits [x]
    (reduce
        (fn [acc x]
            (def y (peek acc))
            (if (and y (= x (rem y 10)))
                (conj (pop acc) (+ x (* y 10)))
                (conj acc x)
            )) [] x))

(defn correct-doubles? [d]
    (reduce
    				; part 1:
        ; (fn [acc x] (if (> x 10) true acc))
        ; part 2:
        (fn [acc x] (if (and (> x 10) (< x 100)) true acc))
        false
        (collapse-digits d)))

(defn is-valid? [x]
    (def d (digits x))
    (and
        (increasing-digits? d)
        (correct-doubles? d)))

(println (reduce
    (fn [sum n] (if (is-valid? n) (+ 1 sum) sum))
    0
    (range MINVAL MAXVAL)))
