; Resolution1
(define/contract (find-the-difference s t)
  (-> string? string? char?)
  (for/first
    ([(k v) (in-hash
              (let ([freq (make-hash)])
                (for ([char-in-s s]) (hash-update! freq char-in-s add1 0))
                (for ([char-in-t t]) (hash-update! freq char-in-t sub1 -1))
                freq))]
     #:when (< v 0))
    k))

; Resolution2
(define/contract (find-the-difference s t)
  (-> string? string? char?)
  (letrec ([solution
            (lambda (xs ys)
              (cond
                [(null? xs) (car ys)]
                [(char=? (car xs) (car ys))
                 (solution (cdr xs) (cdr ys))]
                [else (car ys)]))])
    (solution
      (sort (string->list s) char<?)
      (sort (string->list t) char<?))))
