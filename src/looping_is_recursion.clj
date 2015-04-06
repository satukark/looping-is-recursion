(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (= 0 n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc n]
                 (if (empty? n)
                   acc
                   (recur (first n) (rest n))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [se1 se2]
    (cond
      (and (empty? se1) (empty? se2)) true
      (or (empty? se1) (empty? se2)) false
      (not (= (first se1) (first se2))) false
    :else (recur (rest se1) (rest se2))))]
  (helper seq1 seq2)))


(defn find-first-index [pred a-seq]
  (loop [n 0 se a-seq]
    (cond
      (empty? se) nil
      (pred (first se)) n
      :else (recur (inc n) (rest se)))))

(defn avg [a-seq]
  (loop [n 0 i 0 sqn a-seq]
    (if (empty? sqn)
        (/ i n)
        (recur (inc n) (+ i (first sqn)) (rest sqn)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn parity [a-seq]
  (loop [odds #{} x a-seq]
    (if (empty? x) odds
        (recur (toggle odds (first x))
               (rest x)))))

(defn fast-fibo [n]
  (loop [x 0 y 0 z 1]
    (if (= x n) y
        (recur (inc x) z (+ y z)))))

(defn cut-at-repetition [a-seq]
  (loop [up-to [] in-seq #{} sqn a-seq]
    (let [x (first sqn)]
      (if (or (contains? in-seq x) (empty? sqn)) up-to
          (recur (conj up-to x) (conj in-seq x) (rest sqn))))))


