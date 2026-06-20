;; scanl :: (a -> b -> b) -> b -> List a -> List b
(define (scanl f init xs)
  (let scanl-rec ([init init]
                  [xs xs]
                  [ys (list init)])
    (match xs
      ['() (reverse ys)]
      [(cons x rest)
       (let ([y (f x init)])
         (scanl-rec y rest (cons y ys)))])))

(define/contract (largest-altitude gain)
  (-> (listof exact-integer?) exact-integer?)
  (apply max (scanl + 0 gain)))
