;; scanl :: (a -> b -> b) -> b -> List a -> List b
(define (scanl f init xs)
  (let scanl* ([result (list init)]
               [acc init]
               [xs xs])
    (if (null? xs)
        (reverse result)
        (match-let* ([(cons x rest) xs]
                     [acc* (f x acc)])
          (scanl* (cons acc* result) acc* rest)))))

;; prefix :: Char -> List Char -> Vector Integer
(define (prefix ch xs)
  (list->vector
    (scanl
      (lambda (x acc) (if (char=? x ch) (add1 acc) acc))
      0
      xs)))

(define ((contain-abc? a-prefix b-prefix c-prefix) l r)
  (and
    (> (vector-ref a-prefix (add1 r)) (vector-ref a-prefix l))
    (> (vector-ref b-prefix (add1 r)) (vector-ref b-prefix l))
    (> (vector-ref c-prefix (add1 r)) (vector-ref c-prefix l))))

(define/contract (number-of-substrings s)
  (-> string? exact-integer?)
  (let* ([xs (string->list s)]
         [a-prefix (prefix #\a xs)]
         [b-prefix (prefix #\b xs)]
         [c-prefix (prefix #\c xs)]
         [contain-abc? (contain-abc? a-prefix b-prefix c-prefix)]
         [len (string-length s)])
    (let rec ([l 0]
              [r 0]
              [answer 0])
      (cond
        [(= l len) answer]
        [(and (< r len) (contain-abc? l r)) (rec (add1 l) r (+ answer (- len r)))]
        [(< r (sub1 len)) (rec l (add1 r) answer)]
        [else answer]))))
