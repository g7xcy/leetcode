(define (roman-char-to-int c)
  (case c
    [(#\I) 1] [(#\V) 5] [(#\X) 10]
    [(#\L) 50] [(#\C) 100]
    [(#\D) 500] [(#\M) 1000]))

(define/contract (roman-to-int s)
  (-> string? exact-integer?)
  (letrec
    ([f (lambda (xs)
          (if
            (null? (cdr xs)) (car xs)
            (+
              (f (cdr xs))
              (let ([x (car xs)]
                    [y (cadr xs)])
                (if (< x y) (- x) x)))))])
    (f (map roman-char-to-int (string->list s)))))