#|
t:    2 ... x,  x+1 ... x+y - 1,  x+y,  x+y+1 ... y+limit,  y+limit+1 ... limit+limit
cost: 2     2,  1       1      ,  0  ,  1         1      ,  2             2
diff: 2 0 ...,  -1 0 ...       ,  -1 ,  +1 0 ...         ,  +1 0 ...      0
|#

;; ->pair :: Integer -> Integer -> (Integer, Integer)
(define (->pair x y)
  (if (< x y)
      (list x y)
      (list y x)))

;; vector-update! :: Vector Integer -> Integer -> (Integer -> Integer) -> Void
(define (vector-update! vec pos f)
  (vector-set! vec pos (f (vector-ref vec pos)))
  )

;; ->diff* :: Integer -> (Integer, Integer) -> Vector Integer -> Vector Integer
(define ((->diff* limit) pair diff)
  (let ([x (car pair)]
        [y (cadr pair)])
    (begin
      (vector-update! diff 2 (curry + 2))
      (vector-update! diff (add1 x) sub1)
      (vector-update! diff (+ x y) sub1)
      (vector-update! diff (add1 (+ x y)) add1)
      (vector-update! diff (add1 (+ y limit)) add1)
      diff)))

;; scanl :: (a -> b -> b) -> b -> [a] -> [b]
(define (scanl f init xs)
  (let loop ([acc init]
             [xs xs])
    (if (null? xs)
        (list acc)
        (cons acc
              (loop (f (car xs) acc)
                    (cdr xs))))))

(define (min-moves nums limit)

  (let*-values
    ([(xs ys) (split-at nums (/ (length nums) 2))]
     [(pairs) (foldl (lambda (x y acc) (cons (->pair x y) acc)) '() xs (reverse ys))]
     [(->diff) (->diff* limit)]
     [(diff) (make-vector (+ 2 (* 2 limit)) 0)]
     [(diffs) (vector->list (foldl ->diff diff pairs))]
     [(costs) (scanl + 0 diffs)])
    (apply min
           (take
             (drop costs 3)
             (sub1 (* 2 limit))))))
