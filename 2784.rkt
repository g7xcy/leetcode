;; base-n-freq :: Integer -> Hash Integer Integer
(define (base-n-freq n)
  (foldl
    (lambda (x acc) (hash-update acc x add1 0))
    (hash)
    (append (inclusive-range 1 n) (list n))))

(define/contract (is-good nums)
  (-> (listof exact-integer?) boolean?)
  (let*
    ([max-num (apply max nums)]
     [freq (base-n-freq max-num)])
    (null?
      (filter (compose not zero?)
              (hash-values
                (foldl (lambda (x acc) (hash-update acc x sub1 0)) freq nums))))))
