;; vector-update :: Vector a -> Integer -> (a -> a) -> Vector a
(define (vector-update vec index f)
  (begin
    (vector-set! vec index (f (vector-ref vec index)))
    vec))

;; counting-sort :: List Integer -> Vector Integer
(define (counting-sort xs)
  (let* ([n (length xs)]
         [vec (make-vector (add1 n) 0)])
    (let rec ([vec vec]
              [xs xs])
      (match xs
        ['() vec]
        [(cons x rest) (rec (vector-update vec (min x n) add1) rest)]))))

(define/contract (maximum-element-after-decrementing-and-rearranging arr)
  (-> (listof exact-integer?) exact-integer?)
  (let ([counts (counting-sort arr)]
        [n (length arr)])
    (for/fold ([max-val 0])
              ([i (in-range 1 (add1 n))])
      (min i (+ max-val (vector-ref counts i))))))
