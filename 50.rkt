(define/contract (my-pow x n)
  (-> flonum? exact-integer? flonum?)
  (letrec
    ([f (lambda (x n)
          (cond
            [(zero? n) 1.0]
            [(= n 1) x]
            [(> n 0)
             (let ([sqrt-pow (my-pow x (quotient n 2))])
               (* sqrt-pow sqrt-pow))]
            [else (/ 1 (* x (my-pow x (sub1 (- n)))))]))])
    (if (odd? (abs n))
        (* (f x (sub1 n)) x)
        (f x n))))
