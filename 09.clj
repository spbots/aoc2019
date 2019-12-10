(require '(clojure [string :as str] [test :as test]))
(load-file "./intcode.clj")
(require '[aoc.intcode :as intcode])

(defn parse-int [s] (bigint (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/09") #","))))
(def debug-quine (vec (map parse-int (str/split "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" #","))))
(def debug2 (vec (map parse-int (str/split "1102,34915192,34915192,7,4,7,99,0" #","))))
(def debug3 (vec (map parse-int (str/split "104,1125899906842624,99" #","))))

(test/is (= debug-quine ((intcode/run-program (intcode/init-state debug-quine [])) :o)))
(test/is (= [1219070632396864N] ((intcode/run-program (intcode/init-state debug2 [])) :o)))
(test/is (= [1125899906842624N] ((intcode/run-program (intcode/init-state debug3 [])) :o)))

(defn outputs [i]
    ((intcode/run-program (intcode/init-state file-in i)) :o))
(println (outputs [1]))
(test/is (= [3100786347N] (outputs [1])))

(println (outputs [2]))
(test/is (= [87023N] (outputs [2])))
