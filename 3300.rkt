;; integer/sum :: Integer -> Integer
(define (integer/sum num)
  (let integer/sum-rec ([x num] [acc 0])
    (if (zero? x)
        acc
        (integer/sum-rec (quotient x 10) (+ acc (remainder x 10))))))

(define/contract (min-element nums)
  (-> (listof exact-integer?) exact-integer?)
  (apply min
         (map integer/sum nums)))
