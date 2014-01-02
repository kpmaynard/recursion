(ns recursion)

(defn product [coll]

  (if (empty? coll)
    1
    (* (first coll) (product (rest coll))))
  )

(defn singleton? [coll]
  (and (not (nil? (first coll))) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll))))
  )

(defn general-max [a-seq max-func]

  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max-func (first a-seq) (general-max (rest a-seq) max-func)))))


(defn max-element [a-seq]
  (general-max a-seq max))


(comment (defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
 (if (> (count seq-1) (count seq-2))
   seq-1
   seq-2))

(defn longest-sequence [a-seq]
  (general-max a-seq seq-max)
  )

(defn my-filter [pred? a-seq]

  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
        true
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]


  (if (empty? a-seq)
     '()
     (if (pred? (first a-seq))
       (cons (first a-seq) (my-take-while pred? (rest a-seq)))
       (my-take-while pred? '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
     '()
     (if (pred? (first a-seq))
        (my-drop-while pred? (rest a-seq))
         a-seq)))

(defn seq= [a-seq b-seq]

  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
      false))

  )

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]

  (cond (== n 0) 0
        (== n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]

  (if (or (zero? how-many-times) (neg? how-many-times))
      '()
       (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)))

  )

(defn my-range [up-to]
   (if (or (zero? up-to) (neg? up-to))
     ()

     (let [dec-up-to (dec up-to)] (cons dec-up-to (my-range dec-up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() ())
    (cons (cons (first a-seq) (rest a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]

  (if (empty? a-seq)
    (cons '() ())
    (cons (cons (first a-seq) (rest a-seq) ) (inits (butlast a-seq)))))

(defn rotations [a-seq]

  ;(take (count a-seq) (map #(concat  %1 %2) (tails a-seq) (reverse (inits a-seq))))
  ;(seq (set (map #(concat  %1 %2) (tails a-seq) (reverse (inits a-seq)))))

  (seq (set (map #(concat  %1 %2) (sort #(< (count %1) (count %2)) (tails a-seq)) (sort #(> (count %1) (count %2)) (inits a-seq)))))

  )

(defn my-frequencies-helper [freqs a-seq]
  "freqs a map of keys from the a-seq associated with the counts of their occurrences"
  (if (empty? a-seq)
    freqs
    (let [freq-acc
       (update-in freqs [(first a-seq)] (fnil inc 0))]
       (my-frequencies-helper freq-acc (rest a-seq)))
      ;alternative solution using reduce
      ;(reduce (fn [counts x] (assoc counts x (inc (get counts x 0)))) {} a-seq))
    ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
  )

(defn un-frequencies [a-map]

  (if (empty? a-map)
    '()
    (let [fst (first a-map)
          k   (first fst)
          v   (second fst)]

      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))

    )

  )

(defn my-drop [n coll]

  (if (or (empty? coll) (== n 0))
    (seq coll)
    (my-drop (dec n) (rest coll)


     )
    )

  )

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (vector (my-take mid a-seq) (my-drop mid a-seq))
    ))

(defn seq-merge[a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else     (do
      (if (<= (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq) ))
      )
    )
   )
  )
(defn merge-sort [a-seq]

  (if (or (singleton? a-seq) (== (count a-seq) 0))
    ;sorted
    ;else
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))

  ))

(defn split-into-monotonics [a-seq]
  [:-])

(defn remove-first
  [x xs]
  (if (= x (first xs)) (rest xs) (cons (first xs) (remove-first x (rest xs)))))
(defn perms
  [xs]

  (lazy-seq( if ( seq xs)
  (for [x xs p (perms (remove-first x xs))] (cons x p))
  [[]]))

)

(defn permutations [a-set]

  (perms a-set))

  (comment (cond (empty? a-set) a-set
        (singleton? a-set) a-set
        :else (concat (map #(cons (first %)) (permutations (rest a-set))) (seq (map #(conj %1 (vec (permutations (rest a-set))) (first a-set)))))



         let [
                  split (halve a-set)
                  a    (first split)
                  b     (second split)
                   swap  (vector b a)]
               (vector (apply concat (map permutations split)) (apply concat (map permutations swap))))


  )



(defn powerset [a-set]
  [:-])


(comment
  ;used a helper to rotate the sequence
  (defn x [a-seq]

  (defn x-helper [ a-seq n]

    (if (zero? n)
    '()

     (cons (cons (last a-seq) (butlast a-seq)) (x-helper (cons (last a-seq) (butlast a-seq)) (dec n)))

    )
  )
  (x-helper a-seq (count a-seq))
  )
  )

  (comment
    (
     if (empty? a-seq)
    freqs                                         ;when finished return the freqs map
    ((assoc freqs (first a-seq) (inc (get freqs (first a-seq) 0)))
     (my-frequencies-helper freqs (rest a-seq)) ;recurse with remainder of sequence
      )
    )
  )
