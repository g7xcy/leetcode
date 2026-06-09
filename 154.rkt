;; binary-search :: (Integer -> Integer) -> Integer -> Integer -> Integer
(define (binary-search at left right)
  (let ([mid (quotient (+ left right) 2)])
    (cond
      [(= left right) (at left)]
      [(= (at mid) (at right)) (binary-search at left (sub1 right))]
      [(< (at mid) (at right)) (binary-search at left mid)]
      [else (binary-search at (add1 mid) right)])))

(define/contract (find-min nums)
  (-> (listof exact-integer?) exact-integer?)
  (let ([nums-vec (list->vector nums)])
    (binary-search
      (curry vector-ref nums-vec)
      0
      (sub1 (vector-length nums-vec)))))
