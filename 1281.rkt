(define/contract (subtract-product-and-sum n)
  (-> exact-integer? exact-integer?)
  (letrec
    ([to-list (lambda (x)
                (if (= 0 x)
                    '()
                    (cons (modulo x 10) (to-list (quotient x 10)))
                    ))])
    (let ([digits (to-list n)])
      (-
        (apply * digits)
        (apply + digits)))))
