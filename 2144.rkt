(define (purchase costs)
  (match costs
    ['() 0]
    [(list x) x]
    [(list x y) (+ x y)]
    [(list x y _ rest ...)
     (+ x y (purchase rest))]))

(define/contract (minimum-cost cost)
  (-> (listof exact-integer?) exact-integer?)
  (let ([sorted-costs (sort cost >)])
    (purchase sorted-costs)))
