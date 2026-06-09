#lang racket

(define/contract (smallest-even-multiple n)
  (-> exact-integer? exact-integer?)
  (letrec (
           [gcd
            (lambda (x y)
              (if (= 0 y)
                  x
                  (gcd y (modulo x y))))])
    (quotient (* 2 n) (gcd 2 n))))
