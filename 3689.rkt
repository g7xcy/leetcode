(define/contract (max-total-value nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (* k
     (- (apply max nums) (apply min nums))))
