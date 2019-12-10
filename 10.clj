(require '(clojure [string :as str] [test :as test]))

(def file-lines (str/split (slurp "./input/10") #"\n"))

; expected (x, y): (6, 3), 41 asteroids
(def debug-3 `(".#..#..###"
               "####.###.#"
               "....###.#."
               "..###.##.#"
               "##.##.#.#."
               "....###..#"
               "..#.#..#.#"
               "#..#.#.###"
               ".##...##.#"
               ".....#.#.."))

(defn to-asteroid-set [x]
    (filter some? (for [row (range (count x)) col (range (count (nth x 0)))]
        (when (= (nth (nth x row) col) \#) {:y row, :x col}))))

; for each point, collect a set of angles
(defn angle-sets [c] (map
    (fn [pt0]
        (reduce
            (fn [acc pt1]
                (def dx (- (pt0 :x) (pt1 :x)))
                (def dy (- (pt0 :y) (pt1 :y)))
                (if (= 0 dx dy)
                    acc
                    (conj acc {:angles (conj (acc :angles) (Math/atan2 dy dx))})))
            {:point pt0 :angles #{}}
            c))
    c))

(defn ideal-point [input-lines]
    (reduce
        (fn [acc pt]
            (def n (count (pt :angles)))
            (if (> n (acc :n)) {:point (pt :point) :n n} acc))
        {:point {} :n 0}
        (angle-sets (to-asteroid-set input-lines))))

(def part1 (ideal-point file-lines))
(println part1)
(test/is (= {:x 11 :y 13} (part1 :point)))
(test/is (= 227 (part1 :n)))
