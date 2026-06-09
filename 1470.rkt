#lang racket

; Resolution 1
(define/contract (shuffle nums n)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (foldr
    (lambda (p acc) (cons (car p) (cons (cadr p) acc)))
    '()
    (let*
      ([xs (range n)]
       [ys (map (lambda (x) (+ n x)) xs)])
      (for/list
        ([i xs]
         [j ys])
        (list (list-ref nums i) (list-ref nums j))))))

; Resolution 2
(define/contract (shuffle2 nums n)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (for/foldr
    ([acc '()])
    ([i (range n)]
     [j (map (lambda (x) (+ 3 x)) (range n))])
    (cons (list-ref nums i) (cons (list-ref nums j) acc))))

; Resolution 3
(define/contract (shuffle3 nums n)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (let ([xs (take nums n)]
        [ys (drop nums n)])
    (apply append (map list xs ys))))
