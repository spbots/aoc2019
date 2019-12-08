(require '[clojure.string :as str])

(def w 25)
(def h 6)

(def file-input (partition (* w h) (slurp "./input/08")))

; map to #{0 n0, 1 n1, 2 n2} for each layer
; (println
(defn to-digit-counts [raw-layers]
    (map (fn [in] (reduce
        (fn [acc x]
            (def ix (- (int x) (int \0)))
            (def cur (nth acc ix))
            (assoc acc ix (inc cur)))
        [0 0 0 0 0 0 0 0 0 0] ; digits 0 1 2
        in)) raw-layers))

; part 1:
; find the layer that contains the fewest 0 digits.
; On that layer, what is the number of 1 digits
; multiplied by the number of 2 digits?
(def min-zero-layer (reduce
    (fn [acc x]
        (if (< (nth x 0) (nth acc 0)) x acc))
    (to-digit-counts file-input)))
(println (* (nth min-zero-layer 1) (nth min-zero-layer 2)))

; part 2:
; composite layers. 0 is black, 1 is white, 2 is transparent
(defn to-printable [c]
    (case c
        \0 \.
        \. \.
        \1 \@
        \@ \@
        \2 \2))

(def composite (partition w
    (reduce (fn [acc x]
        (map (fn [img-px layer] (to-printable (if (= img-px \2)
            layer
            img-px))) acc x))
    file-input)))

(doseq [l composite] (println l))
