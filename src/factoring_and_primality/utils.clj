; Common utilities
(ns factoring-and-primality.utils)

(defn isqrt
    "The integer square root of `n`"
    [n]
    (loop [a 1]
        (if (> (* a a)  n)
            (dec a)
            (recur (inc a)))))

(defn divides?
    "True if `m` is divisible by `n`"
    [m n]
    (zero? (mod m n)))