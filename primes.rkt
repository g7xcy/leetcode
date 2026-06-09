;; from :: Integer -> Stream Integer
(define (from n)
  (stream-cons n (from (add1 n))))

;; primes = filterPrime [2..] where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
(define primes
  (letrec ([filter-prime
            (lambda (s)
              (let ([p (stream-first s)]
                    [xs (stream-rest s)])
                (stream-cons
                  p
                  (stream-lazy
                    (filter-prime
                      (stream-filter
                        (lambda (x)
                          (not (zero? (modulo x p))))
                        xs))))))])
    (filter-prime (from 2))))
