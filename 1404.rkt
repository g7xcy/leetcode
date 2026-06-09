;; add1-reversed-chars :: List Char -> List Char
(define (add1-reversed-chars xs)
  (match xs
    [(cons #\0 '()) (list #\1)]
    [(cons #\1 '()) (list #\0 #\1)]
    [(cons #\0 res) (cons #\1 res)]
    [(cons #\1 res) (cons #\0 (add1-reversed-chars res))]))

;; operate :: List Char -> Integer -> Integer
(define (operate xs n)
  (match xs
    ['(#\1) n]
    [(cons #\0 res) (operate res (add1 n))]
    [(cons #\1 res) (operate (add1-reversed-chars xs) (add1 n))]))

(define/contract (num-steps s)
  (-> string? exact-integer?)
  (let ([reversed-num (reverse (string->list s))])
    (operate reversed-num 0)))
