(define/contract (count-odds low high)
  (-> exact-integer? exact-integer? exact-integer?)
  (if (> low high)
      0
      (+ (cond
           [(odd? low) (add1 (count-odds (add1 low) high))]
           [(odd? high) (add1 (count-odds low (sub1 high)))]
           [else (/ (- high low) 2)]))))
