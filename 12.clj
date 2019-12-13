(require '(clojure [string :as str] [test :as test]))

; <x=-9, y=-1, z=-1>
; <x=2, y=9, z=5>
; <x=10, y=18, z=-12>
; <x=-6, y=15, z=-7>

(def initial-state '(
    {:x -9 :y -1 :z -1  :dx 0 :dy 0 :dz 0}
    {:x 2  :y 9  :z 5   :dx 0 :dy 0 :dz 0}
    {:x 10 :y 18 :z -12 :dx 0 :dy 0 :dz 0}
    {:x -6 :y 15 :z -7  :dx 0 :dy 0 :dz 0}))
(def debug-initial-state '(
    {:x -1 :y 0 :z 2 :dx 0 :dy 0 :dz 0}
    {:x 2 :y -10 :z -7 :dx 0 :dy 0 :dz 0}
    {:x 4 :y -8 :z 8 :dx 0 :dy 0 :dz 0}
    {:x 3 :y 5 :z -1 :dx 0 :dy 0 :dz 0}))

(defn new-vel [vel p0 p1]
    (defn delta [axis]
        (def cur (vel (keyword (str "d" axis))))
        (def d (- (p0 (keyword axis)) (p1 (keyword axis))))
        (+ cur (cond (pos? d) -1 (neg? d) 1 (zero? d) 0)))
    {:dx (delta "x") :dy (delta "y") :dz (delta "z")})

(defn update-state [s]
    (map (fn [p0]
        ; 1. apply velocity via gravity
        (def updated-vel (reduce (fn [acc p1] (conj acc (new-vel acc p0 p1))) p0 s))
        ; 2. update position via velocity
        (def updated-pos {
            :x (+ (p0 :x) (updated-vel :dx))
            :y (+ (p0 :y) (updated-vel :dy))
            :z (+ (p0 :z) (updated-vel :dz))})
        (conj updated-vel updated-pos)
        ) s))

(defn map-to-energy [s]
    (map (fn [p] {
        :pot (+ (Math/abs (p :x)) (Math/abs (p :y)) (Math/abs (p :z)))
        :kin (+ (Math/abs (p :dx)) (Math/abs (p :dy)) (Math/abs (p :dz)))}) s))

(defn total-energy [s]
    (reduce (fn [acc p] (+ acc (* (p :pot) (p :kin)))) 0 (map-to-energy s)))

(def dbg-state-seq (iterate update-state debug-initial-state))
(println (nth dbg-state-seq 10))
(println (map-to-energy (nth dbg-state-seq 10)))
(println (total-energy (nth dbg-state-seq 10)))
(test/is (= 179 (total-energy (nth dbg-state-seq 10))))

; part 1
(def state-seq (iterate update-state initial-state))
(println (total-energy (nth state-seq 1000)))
