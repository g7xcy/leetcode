;; diff1? :: Integer -> Integer -> Boolean
(define/contract (diff1? x y)
  (-> exact-integer? exact-integer? boolean?)
  (= 1 (- y x)))

;; intervals :: [Integer] -> [[Integer Integer]]
(define/contract (intervals xs)
  (-> (listof exact-integer?)
      (listof (list/c exact-integer? exact-integer?)))
  (let ([inner
         (foldr
          (lambda (x y acc)
            (if (diff1? x y)
                acc
                (cons (list (add1 x) (sub1 y)) acc)))
          '()
          (drop-right xs 1)
          (cdr xs))])
    (if (= 1 (car xs))
        inner
        (cons (list 1 (sub1 (car xs))) inner))))

;; sum :: Integer -> Integer -> Integer
(define/contract (sum start end)
  (-> exact-integer? exact-integer? exact-integer?)
  (quotient (* (+ start end)
               (add1 (- end start)))
            2))

;; dedup :: [Integer] -> [Integer]
(define/contract (dedup xs)
  (-> (listof exact-integer?) (listof exact-integer?))
  (set->list (list->set xs)))

;; k-sums :: [[Integer Integer]] -> Integer -> Integer -> Integer
(define/contract (k-sums intervals k tail-start)
  (-> (listof (list/c exact-integer? exact-integer?))
      exact-integer?
      exact-integer?
      exact-integer?)
  (cond
    [(zero? k) 0]
    [(null? intervals)
     (sum tail-start
          (sub1 (+ tail-start k)))]
    [else
     (let* ([start (caar intervals)]
            [end (cadar intervals)]
            [len (add1 (- end start))])
       (if (>= len k)
           (sum start
                (sub1 (+ start k)))
           (+ (sum start end)
              (k-sums (cdr intervals)
                      (- k len)
                      tail-start))))]))

(define/contract (minimal-k-sum nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let* ([sorted-nums (sort (dedup nums) <)]
         [max-num (last sorted-nums)]
         [gaps (intervals sorted-nums)]
         [tail-start (add1 max-num)])
    (k-sums gaps k tail-start)))
