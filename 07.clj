(require '(clojure [string :as str] [test :as test]))
(load-file "./intcode.clj")
(require '[aoc.intcode :as intcode])

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/07") #","))))

(defn amp-e-output [mem phases] (reduce
    (fn [acc x]
        (def result (intcode/run-program (intcode/init-state mem [acc x])))
        (nth (result :o) 0))
    0
    phases))

(def part1 (apply max (for [phA (range 5)
                          phB (range 5)
                          phC (range 5)
                          phD (range 5)
                          phE (range 5)]
    (if (= 5 (count (set [phA phB phC phD phE])))
        (amp-e-output file-in [phA phB phC phD phE])
        0
        ))))
(println part1)
(test/is (= 51679 part1))

(defn amp-e-feedback-out [mem phases]
    (def start-states (map #(intcode/init-state mem %) phases)) 
    (println start-states)
)

(def debug-in (vec (map parse-int (str/split "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" #","))))
(def debug-max 139629729)
(println (amp-e-feedback-out debug-in [9 8 7 6 5]))
(test/is (= debug-max (amp-e-feedback-out debug-in [9 8 7 6 5])))
