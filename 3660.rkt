(define (inf xs) (add1 (apply max xs)))

(define (prefix-max xs)
  (cdr
    (reverse
      (foldl
        (lambda (x acc) (cons (max x (car acc)) acc))
        (list (car xs))
        xs))))

(define (suffix-min xs)
  (foldr
    (lambda (x acc) (cons (min x (car acc)) acc))
    (list (inf xs))
    xs))

(define/contract (max-value nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (let ([prefix-max (prefix-max nums)]
        [suffix-min (suffix-min nums)])
    (foldr
      (lambda (p-max s-min ans)
        (if (<= p-max s-min)
            (cons p-max ans)
            (cons (car ans) ans)))
      '()
      prefix-max (cdr suffix-min))))
