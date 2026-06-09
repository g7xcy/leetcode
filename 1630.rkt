;; sublist :: Vector Integer -> Integer -> Integer -> [Integer]
(define (sublist nums l r)
  (if (> l r)
      '()
      (cons
        (vector-ref nums l)
        (sublist nums (add1 l) r))))

;; arithmetic? :: [Integer] -> boolean
(define (arithmetic? xs)
  (if (null? (cdr xs))
      #t
      (let ([q (- (cadr xs) (car xs))])
        (foldl
          (lambda (x y acc) (and acc (= q (- y x))))
          #t
          (drop-right xs 1)
          (cdr xs)))))

;; check-arithmetic-subarrays* :: Vector Integer -> [Integer] -> [Integer] -> [Boolean]
(define (check-arithmetic-subarrays* nums ls rs)
  (if (or (null? ls) (null? rs))
      '()
      (let ([l (car ls)]
            [r (car rs)])
        (cons
          (arithmetic? (sort (sublist nums l r) <))
          (check-arithmetic-subarrays* nums (cdr ls) (cdr rs))))))

(define/contract (check-arithmetic-subarrays nums l r)
  (-> (listof exact-integer?) (listof exact-integer?) (listof exact-integer?) (listof boolean?))
  (check-arithmetic-subarrays* (list->vector nums) l r))
