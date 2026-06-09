(define/contract (transpose matrix)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))
  (apply map list matrix))