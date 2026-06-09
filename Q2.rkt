(define/contract (shuffle nums n)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (let ([xs (take nums n)]
        [ys (drop nums n)])
    (apply append (map list xs ys))))
