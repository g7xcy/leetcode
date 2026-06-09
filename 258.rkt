(define/contract (add-digits num)
  (-> exact-integer? exact-integer?)
  (if (= num 0)
      0
      (add1 (modulo (sub1 num) 9))))
