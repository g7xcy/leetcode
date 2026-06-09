(define (rotate num)
  (case num
    [(0) 0]
    [(1) 1]
    [(6) 9]
    [(8) 8]
    [(9) 6]
    [else -1]))

(define (integer->list x)
  (if (zero? x)
      '()
      (cons (remainder x 10) (integer->list (quotient x 10)))))

(define (contain-common-number? xs)
  (or
    (set-member? xs 2)
    (set-member? xs 3)
    (set-member? xs 4)
    (set-member? xs 5)
    (set-member? xs 7)))

(define/contract (confusing-number n)
  (-> exact-integer? boolean?)
  (if (zero? n)
      #f
      (let ([xs (integer->list n)])
        (and
          (not (contain-common-number? (list->set xs)))
          (not (equal?
                 (map rotate xs)
                 (reverse xs)))))))
