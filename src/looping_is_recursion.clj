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
  ":(")

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

