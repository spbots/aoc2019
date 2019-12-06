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
    (def pos (peek a))
    (def arr (pop a))
    (def opcode (nth arr pos))
    (def modes (arg-modes opcode))
    (defn op-arg [n] (get-arg arr (+ pos n 1) (nth modes n)))
    (defn store-addr [n] (nth arr (+ pos n 1)))

    (case (rem opcode 100) ; 2 digit opcode
        ; add a b store
        1 (conj
            (assoc arr (store-addr 2) (+ (op-arg 0) (op-arg 1)))
            (+ 4 pos))
        ; mult a b store
        2 (conj
            (assoc arr (store-addr 2) (* (op-arg 0) (op-arg 1)))
            (+ 4 pos))
        ; input store
        3 (conj (assoc arr (store-addr 0) input) (+ 2 pos))
        ; output store
        4 (do (println "OUT:" (op-arg 0))
            (conj arr (+ 2 pos)))
        ; jump-true condition instruction-pointer
        5 (conj arr
            (if (not (= 0 (op-arg 0))) (op-arg 1) (+ 3 pos)))
        ; jump-false condition instruction-pointer
        6 (conj arr
            (if (= 0 (op-arg 0)) (op-arg 1) (+ 3 pos)))
        ; lt a b store
        7 (conj
            (assoc arr (store-addr 2) (if (< (op-arg 0) (op-arg 1)) 1 0))
            (+ 4 pos))
        ; eq a b store
        8 (conj
            (assoc arr (store-addr 2) (if (= (op-arg 0) (op-arg 1)) 1 0))
            (+ 4 pos))
        ; end
        99 (conj arr -1)
        ))


(defn run-program [arr input]
    ; use the last element as the instruction pointer
    (loop [arr (conj arr 0)]
        (if (= -1 (peek arr))
            (pop arr)
            (recur (parse-opcode arr input)))))

(def INPUT 5)
(run-program file-in INPUT)
