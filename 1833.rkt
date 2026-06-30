;; index->pair :: Integer -> Pair Integer 0
(define (index->pair index)
  (cons (sub1 index) 0))

;; second* :: (b -> c) -> Pair a b -> Pair a c
(define ((second* f) p)
  (match p
    [(cons x y) (cons x (f y))]))

;; vector-update! :: Vector a -> Integer -> (a -> b) -> Void
(define (vector-update! vec index f)
  (vector-set! vec index (f (vector-ref vec index))))

;; counting-sort/freq :: List Integer -> List (Pair Integer Integer)
(define (counting-sort/freq xs)
  (let ([vec (vector-map index->pair (build-vector 100001 add1))])
    (for ([x xs])
      (vector-update! vec x (second* add1)))
    (vector->list
      (vector-filter (compose positive? cdr) vec))))

(define/contract (max-ice-cream costs coins)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let ([freqs (counting-sort/freq costs)])
    (let buy ([freqs freqs]
              [coins coins]
              [count 0])
      (match freqs
        ['() count]
        [(cons (cons price available) rest)
         (let* ([can-buy (quotient coins price)]
                [buy-count (min available can-buy)])
           (if (= buy-count 0)
               count
               (buy rest
                    (- coins (* buy-count price))
                    (+ count buy-count))))]))))
