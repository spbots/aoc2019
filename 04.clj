(require '[clojure.string :as str])

; number to digits or digits to number (then check range)
(def MINVAL 123257)
(def MAXVAL 647015)

; i can't believe i need to define this...
; (defn exp [base e] (reduce * (repeat e base)))

; https://stackoverflow.com/a/22772070/101645
(defn digits [x]
    (if (< x 10)
        [x]
        (conj (digits (quot x 10)) (rem x 10))))
(println (digits 123))
(println (digits 523783))

(defn increasing-digits? [x]
    (>= (reduce (fn [a b] (if (and
        (>= b a) (>= a 0)) b -1)) 0 x) 0))

(defn has-double? [x]
    (= (reduce (fn [a b] (if (or (= b a) (= a -1)) -1 b)) 0 x) -1))

(println (has-double? [1 2 3 3 4 5]))
(println (increasing-digits? [1 2 3 4 5]))

(defn is-valid? [x]
    (def d (digits x))
    (and
        (increasing-digits? d)
        (has-double? d)))

(println (reduce
    (fn [sum n]
        (if (is-valid? n) (+ 1 sum) sum))
    0
    (range MINVAL MAXVAL)))
