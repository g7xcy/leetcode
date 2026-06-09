;; between :: Integer -> Integer -> Integer -> Boolean
(define ((between target) left right)
  (and
    (<= left target)
    (<= target right)))

;; at :: Vector Integer -> Integer -> Integer
(define ((at nums) index)
  (vector-ref nums index))

;; binary-search :: (Integer -> Integer) -> (Integer -> Integer -> Boolean) -> Integer -> Integer -> Integer -> Integer
(define (binary-search at between target left right)
  (if (> left right)
      -1
      (let* ([mid (quotient (+ left right) 2)]
             [left-val (at left)]
             [mid-val (at mid)]
             [right-val (at right)])
        (cond
          [(= mid-val target) mid]
          [(<= left-val mid-val)
           (if (between left-val mid-val)
               (binary-search at between target left (sub1 mid))
               (binary-search at between target (add1 mid) right))]
          [else
           (if (between mid-val right-val)
               (binary-search at between target (add1 mid) right)
               (binary-search at between target left (sub1 mid)))]))))

(define/contract (search nums target)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let* ([vec (list->vector nums)]
         [left 0]
         [right (sub1 (vector-length vec))]
         [at (at vec)]
         [between (between target)])
    (binary-search at between target left right)))