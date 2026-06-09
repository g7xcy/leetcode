;; ->prefixes :: Integer -> Set Integer
(define (->prefixes num)
  (if (zero? num)
      (set)
      (set-add (->prefixes (quotient num 10)) num)))

;; ->length :: Integer -> Integer
(define (->length num)
  (if (zero? num)
      0
      (add1 (->length (quotient num 10)))))

(define/contract (longest-common-prefix arr1 arr2)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (let* ([prefixes-xs (foldl (lambda (x acc) (set-union (->prefixes x) acc)) (set) arr1)]
         [prefixes-ys (foldl (lambda (x acc) (set-union (->prefixes x) acc)) (set) arr2)]
         [common-prefixes (set->list (set-intersect prefixes-xs prefixes-ys))])
    (if (null? common-prefixes)
        0
        (->length (apply max common-prefixes)))))
