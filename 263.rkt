(define/contract (is-ugly n)
  (-> exact-integer? boolean?)
  (letrec ([reduce
            (lambda (x)
              (cond
                [(= 0 (modulo x 5)) (reduce (quotient x 5))]
                [(= 0 (modulo x 3)) (reduce (quotient x 3))]
                [(= 0 (modulo x 2)) (reduce (quotient x 2))]
                [else (= 1 x)]))])
    (and (> n 0) (reduce n))))
