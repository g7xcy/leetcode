;; freq-ht :: List Integer -> Hash Integer Integer
(define (freq-ht xs)
  (foldl (lambda (x acc) (hash-update acc x add1 0))
         (hash)
         (filter (lambda (x) (> x 1)) xs)))

;; satisfy? :: Hash Integer Integer -> Integer -> Boolean
(define (satisfy? ht key)
  (and (hash-has-key? ht key)
       (>= (hash-ref ht key) 2)
       (hash-has-key? ht (* key key))))

;; maximum-length* :: Hash Integer Integer -> Integer -> Integer
(define (maximum-length* ht x)
  (let ([square-x (* x x)])
    (if (satisfy? ht x)
        (+ 2 (maximum-length* ht square-x))
        1)))

;; count-one :: List Integer -> Integer
(define (count-one xs)
  (let ([cnt (length (filter (lambda (x) (= x 1)) xs))])
    (cond
      [(zero? cnt) 0]
      [(odd? cnt) cnt]
      [else (sub1 cnt)])))

(define/contract (maximum-length nums)
  (-> (listof exact-integer?) exact-integer?)
  (let* ([ht (freq-ht nums)]
         [lengths-without-one (map (curry maximum-length* ht) nums)]
         [lengths (cons (count-one nums) lengths-without-one)])
    (apply max lengths)))
