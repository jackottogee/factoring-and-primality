; An implementation of the Seive of Erathosthenes
; This version works on a boolean array
(ns factoring-and-primality.eratosthenes
    (:require [factoring-and-primality.utils :as utils]))

(defn bool-array 
    "A vector of true values representing integers 2 -> `n`"
    [n] 
    (vec (repeat (dec n) true)))

(defn index 
    "The index of a number in the array defined in [[bool-array]]"
    [p] (- p 2))

(defn replace-value 
    "Replace the `np`-th in `b` value with false (if not already)"
    [b np]
    (assoc b (index np) false))

(defn replace-values
    "Using [[replace-value]], replace all multiples of `p` in `b` with false"
    [b p]
    (if (not (nth b (index p))) 
        b 
        (loop [b' b
               np (+ p p)]
            (if (> (index np) (count b'))
                b'
                (recur (replace-value b' np) (+ np p))))))

(defn sieve
    "Returns a boolean array of primes in range 2 -> `n`"
    [n]
    (let [b (bool-array n)
          ub (utils/isqrt n)]
        
        (loop [b' b
               p  2]
            (if (> p ub)
                b'
                (recur (replace-values b' p) (inc p)))))) 

(defn sieve-integers
    "Returns an array of integer of primes in range 2 -> `n`"
    [n]
    (let [b (sieve n)]
        (map first (filter second (map vector (range 2 n) b)))))