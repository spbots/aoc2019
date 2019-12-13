(require '(clojure [string :as str] [test :as test]))
(load-file "./intcode.clj")
(require '[aoc.intcode :as intcode])

(defn parse-int [s] (bigint (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/11") #","))))

(def start-state {
    :pos {:x 0 :y 0}
    :vel {:x 0 :y 1}   ; velocity vector, +y is "up"
    :painted-panels {} ; mapping of { :pos -> :color }
})

(defn update-vel [vel heading-inc]  {
    :x (if (= 0 (vel :y)) 0 (* (vel :y) heading-inc))
    :y (if (= 0 (vel :x)) 0 (* (vel :x) (- heading-inc)))})

(defn output-handler [pgm]
    (if (= 2 (count (pgm :o)))
        (do
        (def pos ((pgm :state) :pos))
        (def vel (update-vel ((pgm :state) :vel) (if (= 0 (get (pgm :o) 1)) -1 1)))
        (def new-pos {:x (+ (pos :x) (vel :x)) :y (+ (pos :y) (vel :y))})
        (def new-panels (conj ((pgm :state) :painted-panels) {pos (get (pgm :o) 0)}))
        (def new-state (conj (pgm :state) {:pos new-pos :vel vel :painted-panels new-panels}))
        (def panel-color (get ((pgm :state) :painted-panels) new-pos 0))
        (conj pgm {:state new-state :i [panel-color] :o []}))
        pgm))

(def part-1
    (intcode/run-program (intcode/init-state file-in [0] output-handler start-state)))
(println (count ((part-1 :state) :painted-panels)))
(test/is (= 2129 (count ((part-1 :state) :painted-panels))))
