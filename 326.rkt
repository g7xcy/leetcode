(define/contract (is-power-of-three n)
  (-> exact-integer? boolean?)
  (letrec ([power-of-3?
            (lambda (x)
              (cond
                [(= x 1) #t]
                [(and (> x 1) (= (modulo x 3) 0)) (power-of-3? (quotient x 3))]
                [else #f]))])
    (power-of-3? n)))
