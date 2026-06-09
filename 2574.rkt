;; scanl :: (a -> b -> b) -> b -> List a -> List b
(define (scanl f init xs)
  (reverse
    (let rec ([ys (list init)]
              [acc init]
              [xs xs])
      (match xs
        ['() ys]
        [(cons x rest)
         (let ([new-acc (f x acc)])
           (rec (cons new-acc ys) new-acc rest))]))))

;; scanr :: (a -> b -> b) -> b -> List a -> List b
(define (scanr f init xs)
  (let rec ([ys (list init)]
            [acc init]
            [xs (reverse xs)])
    (match xs
      ['() ys]
      [(cons x rest)
       (let ([new-acc (f x acc)])
         (rec (cons new-acc ys) new-acc rest))])))

(define/contract (left-right-difference nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (let ([leftSum (drop-right (scanl + 0 nums) 1)]
        [rightSum (cdr (scanr + 0 nums))])
    (map
      (lambda (x y) (abs (- x y)))
      leftSum
      rightSum)))
