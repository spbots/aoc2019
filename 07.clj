(require '[clojure.string :as str])

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/05") #","))))

(defn init-state [mem inputs] {
    :m mem
    :i inputs
    :o []   ; outputs
    :iptr 0 ; instruction pointer
    })

(defn get-arg [arr pos mode]
    (case mode
        0 (nth arr (nth arr pos)) ; position
        1 (nth arr pos))) ; immediate

(defn arg-modes [opcode]
    ; this can probably be a reduce, but don't worry about it for now.
    [(rem (quot opcode 100) 10)
     (rem (quot opcode 1000) 10)
     (rem (quot opcode 10000) 10)])

(defn parse-opcode [pgm]
    (def arr (pgm :m))
    (def pos (pgm :iptr))
    (def opcode (nth (pgm :m) pos))
    (def modes (arg-modes opcode))
    (defn op-arg [n] (get-arg arr (+ pos n 1) (nth modes n)))
    (defn store-addr [n] (nth arr (+ pos n 1)))

    (defn update-mem [p store-addr x]
        (conj p {:m (assoc (pgm :m) store-addr x)}))
    (defn inc-iptr [n p] (conj p {:iptr (+ n (p :iptr))}))

    (case (rem opcode 100) ; 2 digit opcode
        ; add a b store
        1 (inc-iptr 4 (update-mem pgm (store-addr 2) (+ (op-arg 0) (op-arg 1))))
        ; mult a b store
        2 (inc-iptr 4 (update-mem pgm (store-addr 2) (* (op-arg 0) (op-arg 1))))
        ; input store
        3 (do (def inval (peek (pgm :i)))
          (inc-iptr 2 (update-mem (conj pgm {:i (pop (pgm :i))}) (store-addr 0) inval)))
        ; output store
        4 (do (println "OUT:" (op-arg 0))
            (inc-iptr 2 (conj pgm {:o (conj (pgm :o) (op-arg 0))})))
        ; jump-true condition instruction-pointer
        5 (conj pgm
            {:iptr (if (not (= 0 (op-arg 0))) (op-arg 1) (+ 3 pos))})
        ; jump-false condition instruction-pointer
        6 (conj pgm
            {:iptr (if (= 0 (op-arg 0)) (op-arg 1) (+ 3 pos))})
        ; lt a b store
        7 (inc-iptr 4 (update-mem pgm (store-addr 2) (if (< (op-arg 0) (op-arg 1)) 1 0)))
        ; eq a b store
        8 (inc-iptr 4 (update-mem pgm (store-addr 2) (if (= (op-arg 0) (op-arg 1)) 1 0)))
        ; end (only updates iptr)
        99 (conj pgm {:iptr -1})))


(defn run-program [program]
    ; use the last element as the instruction pointer
    ; end-1 is the size of inputs, then the inputs.
    ; [...program...    12 3 2 (inputs & size)     0 (iptr)]
    (loop [pgm program]
        (if (= -1 (pgm :iptr))
            pgm
            (recur (parse-opcode pgm)))))

; (doseq [phA (range 5)
;         phB (range 5)
;         phC (range 5)
;         phD (range 5)
;         phE (range 5)]
;     (when (= 5 (count (set [phA phB phC phD phE])))
;         (println [phA phB phC phD phE])))
    ; (print-if-magic noun verb))

(def INPUT 5)
(println "NEWOUT:" ((run-program (init-state file-in [INPUT])) :o))
