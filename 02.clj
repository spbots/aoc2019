(require '[clojure.string :as str])

(defn parse-int [s] (Integer. (re-find  #"\d+" s)))
(def file-in (mapv identity (map parse-int (str/split (slurp "./input/02") #","))))

(defn parse-opcode [arr pos]
    (def opcode (nth arr pos))
    (def arg1 (nth arr (nth arr (+ pos 1))))
    (def arg2 (nth arr (nth arr (+ pos 2))))
    (def store-at (nth arr (+ pos 3)))
    (case opcode
        1 (assoc arr store-at (+ arg1 arg2))
        2 (assoc arr store-at (* arg1 arg2))))

; return
(defn get-return-code [arr]
    (loop [arr arr idx 0]
        (if (= 99 (nth arr idx))
            (nth arr 0)
            (recur (parse-opcode arr idx) (+ 4 idx)))))

(defn set-noun-verb [n v] (assoc (assoc file-in 1 n) 2 v))

; replace pos 1 -> 12, 2 -> 2
(println (get-return-code (set-noun-verb 12 2)))

(defn print-if-magic [noun verb]
    (def magic-number 19690720)
    (if (= magic-number (get-return-code (set-noun-verb noun verb)))
        (println (+ verb (* 100 noun)))))

(filterv
    (fn [noun]
        (def print-if-magic-pt (partial print-if-magic noun))
        (filterv print-if-magic-pt (range 100)))
    (range 100))
