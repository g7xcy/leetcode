;; scanl :: (a -> b -> b) -> b -> [a] -> [b]
(define (scanl f init xs)
  (let loop ([acc init]
             [xs xs])
    (if (null? xs)
        (list acc)
        (cons acc
              (loop (f (car xs) acc)
                    (cdr xs))))))

;; vector-update! :: Vector Integer -> Integer -> (Integer -> Integer) -> Void
(define (vector-update! vec index f)
  (vector-set! vec index (f (vector-ref vec index))))

;; common-prefix-num :: Vector Integer -> Integer -> Integer -> Integer
(define (common-prefix-count vec x y)
  (if (= x y)
      1
      (+ (quotient (vector-ref vec x) 2)
         (quotient (vector-ref vec y) 2))))

;; common-prefixes :: Vector Integer -> List Integer -> List Integer -> List Integer
(define (common-prefix-deltas vec xs ys)
  (if (null? xs)
      '()
      (let ([x (car xs)]
            [y (car ys)])
        (vector-update! vec x add1)
        (vector-update! vec y add1)
        (cons (common-prefix-count vec x y) (common-prefix-deltas vec (cdr xs) (cdr ys))))))

(define/contract (find-the-prefix-common-array A B)
  (-> (listof exact-integer?) (listof exact-integer?) (listof exact-integer?))
  (cdr (scanl
         +
         0
         (common-prefix-deltas (make-vector 55 0) A B))))
