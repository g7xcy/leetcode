(define/contract (maximum-wealth accounts)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (apply max (map (lambda (xs) (foldl + 0 xs)) accounts)))