(define (could-change? 5-change 10-change bills)
  (if (null? bills)
      #t
      (let ([bill (car bills)])
        (cond
          [(= bill 20)
           (cond
             [(and (>= 10-change 1) (>= 5-change 1)) (could-change? (sub1 5-change) (sub1 10-change) (cdr bills))]
             [(>= 5-change 3) (could-change? (- 5-change 3) 10-change (cdr bills))]
             [else #f])]
          [(= bill 10)
           (if
             (>= 5-change 1)
             (could-change? (sub1 5-change) (add1 10-change) (cdr bills))
             #f)]
          [else (could-change? (add1 5-change) 10-change (cdr bills))]))))


(define/contract (lemonade-change bills)
  (-> (listof exact-integer?) boolean?)
  (could-change? 0 0 bills))
