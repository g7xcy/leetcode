;; integer->reversed-list :: Integer -> List Integer
(define (integer->reversed-list num)
  (if (zero? num)
      '(0)
      (let rec ([num num])
        (if (zero? num)
            '()
            (cons (remainder num 10) (rec (quotient num 10)))))))

;; peak? :: Integer -> Integer -> Integer -> Boolean
(define (peak? x y z)
  (and (> y x) (> y z)))

;; valley? :: Integer -> Integer -> Integer -> Boolean
(define (valley? x y z)
  (and (< y x) (< y z)))

;; peak-or-valley? :: Integer -> Integer -> Integer -> Boolean
(define (peak-or-valley? x y z)
  (or (peak? x y z)
      (valley? x y z)))

;; count-peak-valley :: List Integer -> Integer -> Integer
(define (count-peak-valley xs count)
  (match xs
    [(list x y z rest ...)
     (count-peak-valley
       (cons y (cons z rest))
       (+ count (if (peak-or-valley? x y z) 1 0)))]
    [_ count]))

(define/contract (total-waviness num1 num2)
  (-> exact-integer? exact-integer? exact-integer?)
  (foldl
    (lambda (num acc) (+ acc (count-peak-valley (integer->reversed-list num) 0)))
    0
    (inclusive-range num1 num2)))
