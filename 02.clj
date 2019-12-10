(require '(clojure [string :as str] [test :as test]))
(load-file "./intcode.clj")
(require '[aoc.intcode :as intcode])

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/02") #","))))

; (defn set-noun-verb [n v] )
(defn output-for-noun-verb [n v]
    (nth ((intcode/run-program
        (intcode/init-state (assoc (assoc file-in 1 n) 2 v) [])) :m) 0))

; replace pos 1 -> 12, 2 -> 2
(def part1 (output-for-noun-verb 12 2))
(println part1)
(test/is (= 4138658 part1))

(def part2 (filter #(= 19690720 (nth % 0))
    (for [noun (range 100) verb (range 100)]
        [(output-for-noun-verb noun verb) (+ verb (* 100 noun))])))
(println part2)
(test/is (= 7264 (nth (nth part2 0) 1)))
