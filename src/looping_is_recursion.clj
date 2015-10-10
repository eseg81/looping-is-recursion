(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [b e acc]
                (if (zero? e)
                 acc
                 (recur b (dec e) (* acc b))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [aux (fn [sq acc]
              (if (empty? sq)
                acc
                (recur (rest sq) (first sq))))]
    (aux a-seq nil)))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1)
             (empty? seq2)) true
        (or (empty? seq1)
            (empty? seq2)) false
        (= (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
        :else false))


(defn find-first-index [pred a-seq]
  (loop [acc 0
         sq a-seq]
    (cond (empty? sq) nil
           (pred (first sq)) acc
           :else (recur (inc acc) (rest sq)))))

(defn avg [a-seq]
  (loop [sum 0
         n_items 0
         sq a-seq]
    (if (empty? sq)
      (/ sum n_items)
      (recur (+ sum (first sq)) (inc n_items) (rest sq)))))

(defn parity [a-seq]
  (loop [sq a-seq
         r-map #{}]
    (cond (empty? sq) r-map
          (contains? r-map (first sq)) (recur (rest sq) (disj r-map (first sq)))
          :else (recur (rest sq) (conj r-map (first sq))))))

(defn fast-fibo [n]
  (loop [n1 1
         n2 0
         i 2]
    (cond (= n 0) 0
          (= n 1) 1
          (= n i) (+ n1 n2)
          :else (recur (+ n1 n2) n1 (inc i)))))



(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         result-sq []
         sq a-seq]
    (if (or (empty? sq)
            (contains? seen (first sq)))
      result-sq
      (recur (conj seen (first sq)) (conj result-sq (first sq)) (rest sq)))))

