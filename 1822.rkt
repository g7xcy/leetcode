(define/contract (array-sign nums)
  (-> (listof exact-integer?) exact-integer?)
  (foldl (lambda (acc x)
           (cond
             [(zero? x) 0]
             [(negative? x) (* acc -1)]
             [else acc]))
         1 nums))
