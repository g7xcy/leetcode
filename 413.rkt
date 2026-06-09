;; continuous-arithmetic :: [Integer] -> [Integer] -> Integer -> Integer -> Integer -> Values [Integer Integer] [Integer] [Integer] Integer
(define (continuous-arithmetic xs ys diff start end)
  (cond
    [(or (null? xs) (null? ys))
     (values
       (list start (sub1 end))
       xs ys end)]
    [(= diff (- (car ys) (car xs)))
     (continuous-arithmetic
       (cdr xs) (cdr ys)
       diff
       start (add1 end))]
    [else (values
            (list start (sub1 end))
            xs ys end)]))

;; count :: Integer -> Integer
(define (count len)
  (if (> len 2)
      (/ (* (sub1 len) (- len 2)) 2)
      0))

;; count-continuous-arithmetics :: [Integer] -> [Integer] -> Integer -> Integer
(define (count-continuous-arithmetics xs ys start)
  (if (or (null? xs) (null? ys))
      0
      (call-with-values
        (lambda () (continuous-arithmetic xs ys (- (car ys) (car xs)) start (add1 start)))
        (lambda (arithmetic-seq new-xs new-ys new-start)
          (+
            (let ([len (add1 (- (cadr arithmetic-seq) (car arithmetic-seq)))])
              (count len))
            (count-continuous-arithmetics new-xs new-ys new-start))))))

(define/contract (number-of-arithmetic-slices nums)
  (-> (listof exact-integer?) exact-integer?)
  (count-continuous-arithmetics (drop-right nums 1) (cdr nums) 0))
