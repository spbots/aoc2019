(require '(clojure [string :as str] [test :as test]))
(load-file "./intcode.clj")
(require '[aoc.intcode :as intcode])

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/13") #","))))

(def initial-state2 [])
(def initial-state {
    :walls []
    :blocks []
    :paddle {}
    :ball {}
    :score 0
})

(defn output-handler [pgm]
    (if (= 3 (count (pgm :o)))
        (do
        (def new-state (case (get (pgm :o) 2)
            1 (conj (pgm :state) {:walls (conj ((pgm :state) :walls) {:x (get (pgm :o) 0) :y (get (pgm :o) 1)})})
            2 (conj (pgm :state) {:blocks (conj ((pgm :state) :blocks) {:x (get (pgm :o) 0) :y (get (pgm :o) 1)})})
            3 (conj (pgm :state) {:paddle {:x (get (pgm :o) 0) :y (get (pgm :o) 1)}})
            4 (conj (pgm :state) {:ball {:x (get (pgm :o) 0) :y (get (pgm :o) 1)}})
            (pgm :state)
        ))
        (conj pgm {:state new-state :o []}))
    pgm))

(def part-1-end
    (intcode/run-program (intcode/init-state file-in [] output-handler initial-state)))
(println (count ((part-1-end :state) :blocks)))
; (println (count (filter #(= 2 (% :id)) (part-1-end :state))))
; (println (count (filter #(= 3 (% :id)) (part-1-end :state))))
; (println (count (filter #(= 4 (% :id)) (part-1-end :state))))
; (println (count (filter #(< 4 (% :id)) (part-1-end :state))))
