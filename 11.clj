(require '(clojure [string :as str] [test :as test]))
(load-file "./intcode.clj")
(require '[aoc.intcode :as intcode])

(defn parse-int [s] (bigint (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/11") #","))))

(def start-state {
    :pos {:x 0 :y 0}
    :vel {:x 0 :y 1}   ; velocity vector, +y is "up"
    :painted-panels {} ; mapping of { :pos -> :color }
    :painted-state 0   ; 0 initial, 1 painted & waiting to turn
})

(defn update-vel [vel heading-inc]  {
    :x (if (= 0 (vel :y)) 0 (* (vel :y) heading-inc))
    :y (if (= 0 (vel :x)) 0 (* (vel :x) (- heading-inc)))})

(defn output-handler [pgm outval]
    (def pos ((pgm :state) :pos))
    ; paint, turn, move
    (case ((pgm :state) :painted-state)
        0 (do ; paint.
            (def new-panels (conj ((pgm :state) :painted-panels) {pos outval}))
            (def new-state (conj (pgm :state) {:painted-panels new-panels :painted-state 1}))
            (conj pgm {:state new-state})
        )
        1 (do ; adjust velocity, move, and set input to new color
            (def vel (update-vel ((pgm :state) :vel) (if (= 0 outval) -1 1)))
            (def new-pos {:x (+ (pos :x) (vel :x)) :y (+ (pos :y) (vel :y))})
            (def new-state (conj (pgm :state) {:pos new-pos :vel vel :painted-state 0}))
            (def panel-color (get ((pgm :state) :painted-panels) new-pos 0))
            (conj pgm {:state new-state :i [panel-color]})
        )
    )
    ; (def paint-color (nth outval 0))
    ; (def heading-adjust (nth outval 1))
    ; (println outval state)
    ; pgm
)

(def part-1
    (intcode/run-program (intcode/init-state file-in [0] output-handler start-state)))
(println (count ((part-1 :state) :painted-panels)))
(test/is (= 2129 (count ((part-1 :state) :painted-panels))))
