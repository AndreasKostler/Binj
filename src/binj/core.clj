(ns binj.core)

(defn decimal-to-bytelist 
          ([val] (decimal-to-bytelist val 8 1))
        ([val bits] (decimal-to-bytelist val bits 1))
        ([val bits unit]  
              (loop [bits (* bits unit) 
                 val val
                 acc '()]
                 (if (<= bits 8)
                     (conj acc (bit-and val (dec (bit-shift-left 1 bits))))
                     (recur (- bits 8) (bit-shift-right val 8) (conj acc (bit-and val 0xff)))))))


(defn- reduce-bl [bl]
        (reduce + (map #(* (bit-and %1 0xff) %2)
                           bl
                           (iterate #(bit-shift-left % 8) 1))))

(defn bytelist-to-decimal
          ([bytelist] (reduce-bl (reverse bytelist)))
          ([bytelist n] (reduce-bl (take n (reverse bytelist)))))

(defn << [& rest]
            (let [rest 
              (if (and (= (count rest))
                       (string? (first rest)))
                  (map int (first rest))
                  rest)]
                  (flatten (map (fn [val]
                                (if (seq? val)
                                    (apply decimal-to-bytelist val)
                                    (bit-and val 0xff))) rest))))



(defn prepare-binding-vec [bindings]
        (let [bdg-pair (partition 2 bindings)]
             (loop [bdg bdg-pair
                acc []]
                 (if (empty? bdg) acc
                    (let [b (first bdg)
                         bin (first b)
                         val (second b)]
                         (if (and (vector? bin)
                                  (some number? bin))
                             (recur (rest bdg)
                                    (into acc (destructure-binvec b)))
                             (recur
                              (rest bdg)
                              (conj (conj acc bin) val))))))))


(defmacro bin-let [bindings & body]
  (let [bindings (prepare-binding-vec bindings)]
                     `(let [~@(destructure bindings)] ~@body)))

