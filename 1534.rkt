#lang racket

(define/contract (count-good-triplets arr a b c)
  (-> (listof exact-integer?) exact-integer? exact-integer? exact-integer? exact-integer?)
  (for*/sum ([(x ix) (in-indexed arr)]
             [(y iy) (in-indexed arr)]
             [(z iz) (in-indexed arr)]
             #:when (and
                      (< ix iy)
                      (< iy iz)
                      (<= (abs (- x y)) a)
                      (<= (abs (- y z)) b)
                      (<= (abs (- x z)) c)))
    1))