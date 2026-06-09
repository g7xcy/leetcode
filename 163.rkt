(define/contract (find-missing-ranges nums lower upper)
  (-> (listof exact-integer?) exact-integer? exact-integer? (listof (listof exact-integer?)))
  (cond
    [(> lower upper) '()]
    [(null? xs) (list (list lower upper))]
    [else
     (let ([x (car nums)])
       (if (= x lower)
           (find-missing-ranges (cdr nums) (add1 lower) upper)
           (cons
             (list lower (sub1 x))
             (find-missing-ranges (cdr nums) (add1 x) upper))))]))
