(define/contract (count-primes n)
  (-> exact-integer? exact-integer?)
  (if (<= n 2)
      0
      (let ([is-prime (make-vector n #t)])
        (vector-set! is-prime 0 #f)
        (vector-set! is-prime 1 #f)

        (for ([i (in-range 2 n)]
              #:break (>= (* i i) n))
          (when (vector-ref is-prime i)
            (for ([j (in-range (* i i) n i)])
              (vector-set! is-prime j #f))))

        (for/sum ([b (in-vector is-prime)])
          (if b 1 0)))))
