(define (all-eq xs)
  (andmap (lambda (x) (= x (car xs))) xs))

(define/contract (min-operations grid x)
  (-> (listof (listof exact-integer?)) exact-integer? exact-integer?)
  (let ([flat-ys (flatten grid)])
    (if (not (all-eq (map (lambda (y) (modulo y x)) flat-ys)))
        -1
        (let* ([sorted-ys (sort flat-ys <)]
               [half (list-ref sorted-ys (quotient (length sorted-ys) 2))])
            (apply + (map (lambda (y) (/ (abs (- y half)) x)) sorted-ys))))))
