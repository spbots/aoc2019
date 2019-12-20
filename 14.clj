(require '(clojure [string :as str] [test :as test]))
(load-file "./intcode.clj")
(require '[aoc.intcode :as intcode])

; plan:
; split the line at the =>
;   - use the second part as a number and keyword (e.g. 1 :FUEL)
;   - parse the first part as a collection of the same (3 :A 4 :B)
; store in map {:FUEL {:out 1 :in {:A 3 :B 4} }}
; add info to the map: number of reactions to get from ORE to :chem
; starting at fuel, replace the chemicals with the largest number of
;   reactions and replacing chems at the maximum level.
; e.g. annotating the next input with distances for the sample input
;   shows that FUEL is d3, meaning that we need to first replace all
;   the d2 ingredients, then d1 ingredients.
(def debug-in2 (str ; 13312 ORE for 1 FUEL
"157 ORE => 5 NZVS\n" ; d 1
"165 ORE => 6 DCFZ\n" ; d 1
"44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n" ; d 3
"12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n" ; d 2
"179 ORE => 7 PSHF\n" ; d 1
"177 ORE => 5 HKGWZ\n" ; d 1
"7 DCFZ, 7 PSHF => 2 XJWVT\n" ; d 2
"165 ORE => 2 GPVTF\n" ; d 1
"3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT\n")) ; d 2

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(defn str-to-chem [s] {(keyword (nth (str/split s #" ") 1)) (parse-int (nth (str/split s #" ") 0))})
(defn input-to-dict [s]
    (reduce (fn [acc x]
        (def in-and-out (filter
            (fn [y] (< 0 (count y)))
            (map str/trim (str/split x #"[=>]"))))
        (def out (str-to-chem (nth in-and-out 1)))
        (def inputs (reduce
            (fn [acc x] (conj acc (str-to-chem x)))
            {}
            (map str/trim (str/split (nth in-and-out 0) #","))))
        (conj acc {(apply key out) {:n (apply val out) :in inputs}}))
    {}
    (str/split s #"\n")))

(defn distance-from-ore [dict chem]
    (cond
        (not (dict chem)) 0 ; not in dict, this is probably :ORE
        ((dict chem) :d) ((dict chem) :d) ; existing distance
        :else (+ 1 (apply max (map
            #(distance-from-ore dict (get % 0))
            ((dict chem) :in))))))

(defn add-distances-from-ore [dict]
    (reduce (fn [acc x]
        (def chem-name (get x 0))
        (def chem-details (get x 1))
        (assoc acc chem-name (assoc chem-details :d (distance-from-ore dict chem-name))))
    dict
    dict))

(def dbg-init-state (add-distances-from-ore (input-to-dict debug-in2)))
(def init-state (add-distances-from-ore (input-to-dict (slurp "./input/14"))))

(test/is (= 1 ((dbg-init-state :PSHF) :d)))
(test/is (= 2 ((dbg-init-state :XJWVT) :d)))
(test/is (= 3 ((dbg-init-state :FUEL) :d)))

(defn reduce-to-ore [dict chem n]
    (defn max-dist [inputs] (reduce (fn [acc x] (max acc (distance-from-ore dict (key x)))) 0 inputs))
    (defn inputs-for [chem n]
        (def n-produced ((dict chem) :n))
        (def times-to-run-reaction (+ (quot n n-produced) (if (= 0 (mod n n-produced)) 0 1)))
        (defn scale-amount-by [n]
            (fn [acc x] (assoc acc (get x 0) (* n (get x 1)))))
        (reduce (scale-amount-by times-to-run-reaction) {} ((dict chem) :in)))

    ; substitute all the chemicals that are at the max distance until distance 0
    (loop [inputs (inputs-for chem n)]
        (if (= 1 (count inputs))
            inputs
            (do
                (def d (max-dist inputs))
                (def in-remaining (filter #(< (distance-from-ore dict (key %)) d) inputs))
                (def in-to-replace (filter #(= (distance-from-ore dict (key %)) d) inputs))
                (recur (reduce
                    (fn [acc x] (merge-with + acc (inputs-for (key x) (val x))))
                    (into {} in-remaining)
                    in-to-replace))))))

(defn n-ore-for-fuel [n]
    ((reduce-to-ore init-state :FUEL n) :ORE))

(println (n-ore-for-fuel 1))
(test/is (= 741927 (n-ore-for-fuel 1)))

; max guess: 1e13 / 741927 =>
;   13478414 fuel requires 5683017593433 ore
(println (n-ore-for-fuel 13478414))

; disappointing: I was hoping that the lazy collections
; would allow for things like binary search to only evaluate
; the collection at certain indices, but doing something like
;
; (println (+ 402169 (java.util.Collections/binarySearch
;     (map n-ore-for-fuel (range 402169 4021690))
;     1000000000000)))
;
; requires evaluating all the elements up until the pivot.
; instead of this, rip off the binary search algorithm from
; https://rosettacode.org/wiki/Binary_search#Clojure
; and adapt it to find the one that is within 741927 ore.
(defn bsearch
    ([coll t] (bsearch coll 0 (dec (count coll)) t))
    ([coll l u t]
    (def ore-for-one-fuel (n-ore-for-fuel 1))
    (if (> l u) -1
        (let [m (quot (+ l u) 2)
              mth (nth coll m)
              n-ore (n-ore-for-fuel mth)]
        (cond
          ; the ore required is greater than 1e12
          ; so search the lower half
          (> n-ore t) (recur coll l (dec m) t)
          ; the ore required is less than 1e12
          ; but adding one fuel worth of ore is greater
          (and (< n-ore t) (>= (+ n-ore ore-for-one-fuel) t)) m
          ; the ore required is less than 1e12
          ; so search the upper half
          (< n-ore t) (recur coll (inc m) u t)
          ; we've found our target
          ; so return its index
          (= n-ore t) m
          )))))

(println (+ 1 (bsearch (range 1 13478414) 1000000000000)))
(println (n-ore-for-fuel 2371699))
