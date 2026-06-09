#lang racket

(define/contract (xor-operation n start)
  (-> exact-integer? exact-integer? exact-integer?)
  (foldr
    bitwise-xor
    0
    (map (lambda (x) (+ start (* 2 x))) (range n))))