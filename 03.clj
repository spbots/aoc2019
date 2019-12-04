(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-int [s] (Integer. (re-find  #"\d+" s)))
(def file-in (str/split (slurp "./input/03") #"\n"))
(def path-one (str/split (nth file-in 0) #","))
(def path-two (str/split (nth file-in 1) #","))

; approach:
; for each path, save a bitmap around the origin and
; check for the closest intersection. if there's no
; intersection, increase the "radius" and run again.

; approach 2:
; convert each path into a vector of coords visited
; convert vectors to sets
; find union of the sets
; find shortest distance in the union

(defn increment-vector-from-mvmt [mvmt]
    (case (first mvmt) \L [-1 0] \R [1 0] \U [0 1] \D [0 -1]))

(defn path-to-coord-vec [path mvmt]
    (def d (increment-vector-from-mvmt mvmt))
    (def start (peek path))
    (reduce
        (fn [p n] (conj
            p
            [(+ (first start) (* n (first d))) (+ (peek start)  (* n (peek d)))]))
        path
        (into [] (range 1 (+ 1(parse-int mvmt))))))

(def set-one (into #{} (reduce path-to-coord-vec [ [0 0] ] path-one)))
(def set-two (into #{} (reduce path-to-coord-vec [ [0 0] ] path-two)))

(println "intersections" (set/intersection set-one set-two))

; find the second smallest distance (smallest is zero)
(defn abs [x] (max x (- x)))
(defn manhattan-dist [p] (+ (abs (first p)) (abs (peek p))))

(println "min distance"
    (reduce
    (fn [min point]
        (def d (manhattan-dist point))
        (if (and (< d min) (> d 0)) d min))
    100000
    (set/intersection set-one set-two)))
