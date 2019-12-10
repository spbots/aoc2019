(require '(clojure [string :as str] [test :as test]))
(load-file "./intcode.clj")
(require '[aoc.intcode :as intcode])

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/05") #","))))

(defn diagnostic-out [i]
    (peek ((intcode/run-program (intcode/init-state file-in [i])) :o)))

(println (diagnostic-out 1))
(test/is (= 15426686 (diagnostic-out 1)))

(println (diagnostic-out 5))
(test/is (= 11430197 (diagnostic-out 5)))
