(require '[clojure.string :as str])

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/05") #","))))

(defn get-arg [arr pos mode]
    (case mode
        0 (nth arr (nth arr pos)) ; position
        1 (nth arr pos))) ; immediate

(defn arg-modes [opcode]
    ; this can probably be a reduce, but don't worry about it for now.
    [(rem (quot opcode 100) 10)
     (rem (quot opcode 1000) 10)
     (rem (quot opcode 10000) 10)])

(defn parse-opcode [a input]
    ; (println a)
    (def pos (peek a))
    (def arr (pop a))
    (def opcode (nth arr pos))
    (def modes (arg-modes opcode))
    (case (rem opcode 100) ; 2 digit opcode
        1 (conj (assoc arr (nth arr (+ pos 3))
            (+ (get-arg arr (+ pos 1) (nth modes 0))
               (get-arg arr (+ pos 2) (nth modes 1))))
            (+ 4 pos))
        2 (conj (assoc arr (nth arr (+ pos 3))
            (* (get-arg arr (+ pos 1) (nth modes 0))
               (get-arg arr (+ pos 2) (nth modes 1))))
            (+ 4 pos))
        3 (do
            (println "IN")
            (conj (assoc arr (nth arr (+ pos 1)) input) (+ 2 pos)))
        4 (do
            (println "OUT:" (get-arg arr (+ pos 1) (nth modes 0)))
            (conj arr (+ 2 pos)))
        99 (conj arr -1)
        ))

; (println (parse-opcode `[1001 4 3 4 33 0] 1))
; (println (parse-opcode `[11001 4 3 4 33 0] 1))
; (println (parse-opcode `[1101 4 3 4 33 0] 1))
; (println (parse-opcode `[101 4 3 4 33 0] 1))

(defn run-program [arr input]
    ; use the last element as the instruction pointer
    (loop [arr (conj arr 0)]
        (if (= -1 (peek arr))
            (pop arr)
            (recur (parse-opcode arr input)))))

(def INPUT 1)
(run-program file-in INPUT)
