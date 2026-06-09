;; split-digit-reversed :: Integer -> [Integer]
(define (split-digit-reversed num)
  (if (zero? num)
      '()
      (call-with-values
        (lambda () (quotient/remainder num 10))
        (lambda (q r) (cons r (split-digit-reversed q))))))

;; split-digit :: Integer -> [Integer]
(define split-digit (compose reverse split-digit-reversed))

;; separate-digits :: [Integer] -> [Integer]
(define/contract (separate-digits nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (append-map split-digit nums))
  