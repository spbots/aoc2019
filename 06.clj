(require '[clojure.string :as str])

(def file-lines (str/split (slurp "./input/06") #"\n"))
(def debug-lines ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"])

; game plan
; make a dict, e.g. F5T)A3Q
; { :A3Q (directly-orbits) :F5T] }
(defn add-line-to-hash-map [hmap l]
    (def tokens (str/split l #"\)"))
    (assert (= 2 (count tokens)))
    (conj hmap {(keyword (nth tokens 1)) (keyword (nth tokens 0))}))

(defn path-from-core [hmap id]
    (if id (conj (path-from-core hmap (hmap id)) id) []))

(defn calc-distance [hmap id]
    (+ -1 (count (path-from-core hmap id))))

(def dmap (reduce add-line-to-hash-map {} debug-lines))
(def realmap (reduce add-line-to-hash-map {} file-lines))

; part 1
(println (reduce
    (fn [acc id] (+ acc (calc-distance realmap id)))
    0
    (keys realmap)))

(def path-you (path-from-core realmap :YOU))
(def path-san (path-from-core realmap :SAN))
(def common-path-len (count (filterv identity
    (mapv #(= %1 %2) path-you path-san))))

; part 2
(println (+ (count path-you) (count path-san) (- (* 2 (+ 1 common-path-len)))))
