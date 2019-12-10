(ns aoc.intcode)

(defn get-arg [pgm pos mode]
    (def arr (pgm :m))
    (case mode
        0 (nth arr (nth arr pos))                   ; position
        1 (nth arr pos)                             ; immediate
        2 (nth arr (+ (nth arr pos) (pgm :rbase)))  ; relative
        ))

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
    (defn op-arg [n] (get-arg pgm (+ pos n 1) (nth modes n)))
    (defn store-addr [n]
        (case (nth modes n)
            0 (nth arr (+ pos n 1))
            2 (+ (pgm :rbase) (nth arr (+ pos n 1)))))

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
        4 (do ;(println "OUT:" (op-arg 0))
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
        ; inc-rbase amount
        9 (inc-iptr 2 (conj pgm {:rbase (+ (pgm :rbase) (op-arg 0))}))
        ; end (only updates iptr)
        99 (conj pgm {:iptr -1})))

; return the state of the program once the :iptr is -1
(defn run-program [program]
    (loop [pgm program]
        (if (= -1 (pgm :iptr)) pgm (recur (parse-opcode pgm)))))

(defn init-state [mem inputs] {
    :m (vec (flatten (conj mem (repeat 10000 0))))
    :i inputs ; input vector
    :o []     ; outputs
    :iptr 0N  ; instruction pointer
    :rbase 0N ; relative base offset
    })
