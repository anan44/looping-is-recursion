(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc x n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) base (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper 
        (fn [acc seq]
          (if (empty? seq)
            acc
            (recur (first seq) (rest seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper
        (fn [acc a-seq b-seq]
          (if (and (empty? a-seq) (empty? b-seq))
            acc
            (recur (and (= (first a-seq) (first b-seq))
                        (= (count a-seq) (count b-seq)))
                   (rest a-seq) (rest b-seq))))]
    (helper true seq1 seq2)))


(defn find-first-index [pred a-seq]
 (loop [acc 0
        se  a-seq]
   (cond
    (empty? se) nil
    (pred (first se)) acc
    :else (recur (inc acc) (rest se)))))


(defn loopy-factorial [down-from]
  (loop [acc 1 
         n down-from]
    (if (zero? n)
      acc
      (recur (* acc n) (dec n)))))


(defn avg [a-seq]
  (loop [co 0
         sum 0
         se a-seq]
    (if (empty? se)
      (/ sum co)
      (recur (inc co) (+ sum (first se)) (rest se)))))

(defn parity [a-seq]
  (loop [acc #{}
         se a-seq]
    (if (empty? se)
      acc
      (if (contains? acc (first se))
        (recur (disj acc (first se)) (rest se))
        (recur (conj acc (first se)) (rest se))))))

(defn fast-fibo [n]
  (loop [acc 0
         acc2 1
         fib n]
    (if (zero? fib)
      acc
      (recur acc2 (+ acc2 acc) (dec fib)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         se a-seq]
    (cond
     (contains? (set acc) (first se)) acc
     (empty? se) acc
     :else (recur (conj acc (first se)) (rest se)))))


