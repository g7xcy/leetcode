(define/contract (plus-one digits)
  (-> (listof exact-integer?) (listof exact-integer?))
  ; acc: ((value . carry))
  (let ([ans (map
               car
               (take (foldl
                       (lambda
                         (vaule acc)
                         (let ([carry (cdar acc)])
                           (cons (cons (remainder (+ vaule carry) 10) (quotient (+ vaule carry) 10)) acc)))
                       (list (cons 0 1))
                       (reverse (cons 0 digits))) (add1 (length digits))))])
    (if (zero? (car ans))
        (cdr ans)
        ans)))
