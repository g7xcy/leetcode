#lang racket

(define/contract (num-identical-pairs nums)
  (-> (listof exact-integer?) exact-integer?)
  (foldr +
         0
         (hash-map
           (foldl
             (lambda (x h) (hash-update h x add1 0))
             (hash) nums)
           (lambda (_ v) (quotient (* v (- v 1)) 2)))))
