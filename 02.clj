(require '[clojure.string :as str])

(defn parse-int [s] (Integer. (re-find  #"-?\d+" s)))
(def file-in (vec (map parse-int (str/split (slurp "./input/02") #","))))

(defn parse-opcode [a]
    (def pos (peek a))
    (def arr (pop a))
    (def opcode (nth arr pos))
    (def arg1 (nth arr (nth arr (+ pos 1))))
    (def arg2 (nth arr (nth arr (+ pos 2))))
    (def store-at (nth arr (+ pos 3)))
    (case opcode
        1 (conj (assoc arr store-at (+ arg1 arg2)) (+ 4 pos))
        2 (conj (assoc arr store-at (* arg1 arg2)) (+ 4 pos))))

(defn run-program [arr]
    ; use the last element as the instruction pointer
    (loop [arr (conj arr 0)]
        (def idx (peek arr))
        (if (= 99 (nth arr idx))
            (pop arr)
            (recur (parse-opcode arr)))))

(defn set-noun-verb [n v] (assoc (assoc file-in 1 n) 2 v))

; replace pos 1 -> 12, 2 -> 2
(println (nth (run-program (set-noun-verb 12 2)) 0))

(defn print-if-magic [noun verb]
    (def magic-number 19690720)
    (if (= magic-number (nth (run-program (set-noun-verb noun verb)) 0))
        (println (+ verb (* 100 noun)))))

(doseq [noun (range 100)
        verb (range 100)]
    (print-if-magic noun verb))
