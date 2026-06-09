;; binary-search :: Vector Integer -> Integer -> Integer -> Integer
(define (binary-search nums left right)
  (let* ([mid (quotient (+ left right) 2)]
         [num-mid (vector-ref nums mid)]
         [num-right (vector-ref nums right)])
    (cond
      [(= left right) num-right]
      [(> num-mid num-right) (binary-search nums (add1 mid) right)]
      [else (binary-search nums left mid)])))

(define/contract (find-min nums)
  (-> (listof exact-integer?) exact-integer?)
  (let ([nums-vec (list->vector nums)])
    (binary-search
      nums-vec
      0
      (sub1 (vector-length nums-vec)))))