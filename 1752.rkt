;; List Integer -> Integer
(define (decrease-num xs)
  (match xs
    ['() 0]
    [(list _) 0]
    [(list-rest x y rest)
     (+ (if (> x y) 1 0)
        (decrease-num (cons y rest)))]))

(define/contract (check nums)
  (-> (listof exact-integer?) boolean?)
  (let ([xs (append nums (list (car nums)))])
    (<= (decrease-num xs) 1)))
