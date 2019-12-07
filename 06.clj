(require '[clojure.string :as str])

(def file-lines (str/split (slurp "./input/06") #"\n"))
(def debug-lines ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"])

; game plan
; make a dict, e.g. F5T)A3Q
; { :A3Q [:dist -1, :directly-orbits F5T] }
(defn add-line-to-hash-map [hmap l]
    (def tokens (str/split l #"\)"))
    (assert (= 2 (count tokens)))
    (conj hmap [(keyword (nth tokens 1)) [0 (keyword (nth tokens 0))]]))

(defn calc-distances [hmap satellite-id]
    (def satellite (hmap satellite-id))
    ; if it exists, increment the distance and the distance of ancestors
    ; if not, then it is an absolute center
    (if satellite
        (do
            (def dist (nth satellite 0))
            (def center-id (nth satellite 1))

            ; !!!!! for some reason this breaks if done in the assoc?????
            ; i.e. the following is broken:
            ; (assoc (calc-distances hmap center-id) satellite-id (assoc satellite 0 (+ dist 1)))
            ; is it that the recursive call (calc-distances) must be on the outside?
            (def newval (assoc satellite 0 (+ dist 1)))
            (def hmap2 (assoc hmap satellite-id newval))

            (calc-distances hmap2 center-id))
            hmap))

(def dmap (reduce add-line-to-hash-map {} debug-lines))
(def realmap (reduce add-line-to-hash-map {} file-lines))
(println
    (reduce
    (fn [acc x] (+ (nth (nth x 1) 0) acc))
    0
    (reduce calc-distances realmap (keys realmap))))
    ; (reduce calc-distances dmap (keys dmap))))










; or what about reversed:
; "TK3)TVH\nTK3)UUH\nUUH)YIP\n"
; {
;   :TK3 [:dist 0, :satellites [TVH, UUH]],
;   :TVH [:dist 0, :satellites []],
;   :UUH [:dist 0, :satellites [YIP]],
;   :YIP [:dist 0, :satellites []]
; }
; (defn add-center-with-satellites [hmap center-id satellite-ids]
;     (def existing-center (hmap center-id))
;     (if existing-center
;         (conj hmap [center-id (into existing-center satellite-ids)])
;         (conj hmap [center-id (into [-1] satellite-ids)])))

; (defn add-line-to-hash-map [hmap l]
;     (def tokens (str/split l #"\)"))
;     (assert (= 2 (count tokens)))
;     (def center-id (keyword (nth tokens 0)))
;     (def satellite-id (keyword (nth tokens 1)))
;     (add-center-with-satellites
;         (add-center-with-satellites hmap center-id [satellite-id])
;         satellite-id
;         []))

; (println (reduce add-line-to-hash-map {} file-lines))

; go through all the keys, for each key, look up the satellites and
;   > increment their distance by 1
;   > go through their satellites and repeat
; ** THIS DOESN'T WORK SINCE IT RELIES ON TRAVERSING IN ORDER **
; (defn increment-dist [hmap id]
;     (def dist (nth (hmap id) 0))
;     (def satellites (subvec (hmap id) 1))
;     (println "inc:" id dist satellites "\t" hmap)
;     ; update the satellites, then update this.
;     (conj (reduce increment-dist hmap satellites)
;         [id (assoc (hmap id) 0 (+ 1 dist))]))

; (def hmap (reduce add-line-to-hash-map {} [
    ; total 42
    ; "COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"
;     "C)D" "C)E" "D)F" "A)B" "A)C"
;     ]))
; (println hmap)
; (def k (keys hmap))
; (println k)
; (println
;     (reduce
;     (fn [acc x] (do (println "x:" x) (+ acc (nth (nth x 1) 0))))
;     0
;     (reduce increment-dist hmap k)))

; (def file-map (reduce add-line-to-hash-map {} file-lines))
; (println
;     (reduce
;     (fn [acc x] (+ acc (nth (nth x 1) 0)))
;     0
;     (reduce increment-dist file-map (keys file-map))))
