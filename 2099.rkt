;; indexed :: [Integer] -> [(Integer Integer)]
(define (indexed xs)
  (for/list ([(x i) (in-indexed xs)])
    (cons x i)))

;; lift2 :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(define (lift2 g f)
  (lambda (x y)
    (g (f x) (f y))))

(define/contract (max-subsequence nums k)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (let* ([indexed-nums (indexed nums)]
         [sorted-nums (sort indexed-nums (lift2 > car))]
         [seq (take sorted-nums k)])
    (map car
         (sort seq (lift2 < cdr)))))
