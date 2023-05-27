; Basic trial division algorithm
(ns factoring-and-primality.trial-division
    (:require [factoring-and-primality.utils :as utils]))

(defn primality
    "Tests the primality of a number through trial division"
    [n]
    (let [ub (utils/isqrt n)]
        (loop [m 2]
            (if (utils/divides? m n)
                false
                (if (> m ub)
                    true
                    (recur (inc m)))))))

(defn divide
    "Divides `n` by `p` continuously and returns {`p` `i`} s.t. `p`^`i`*`m` = `n`"
    [n p]
    (loop [m n
           i 0]
        (if (not (utils/divides? m p))
            (assoc {} p i)
            (recur (/ m p) (inc i)))))


(defn factor
    "Completely factors `n` via trial division."
    [n]
    (let [ub (utils/isqrt n)]
        (loop [m n 
               p 2
               factors {}]
            (if (or (> p ub) (= m 1))
                (if (> m 1)                                         ; not completely factored yet
                    (assoc factors m 1)
                    factors)
                (let [divs? (utils/divides? m p)]
                    (recur (if divs?
                           (loop [l m]                              ; TO-DO find way to do this once, not twice in divide and here
                                  (if (not (utils/divides? l p)) 
                                      l
                                      (recur (/ l p))))
                           m)
                           (inc p)
                           (if divs?
                            (merge factors (divide m p))
                            factors)))))))