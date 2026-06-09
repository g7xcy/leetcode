;; indexed :: [Integer] -> [(Integer, Integer)]
(define (indexed xs)
  (if (null? xs)
      '()
      (reverse (foldl
                 (lambda (x acc) (cons (list x (add1 (cadar acc))) acc))
                 (list (list (car xs) 0))
                 (cdr xs)))))

(define/contract (minimum-deletions nums)
  (-> (listof exact-integer?) exact-integer?)
  (match-let* (
               [len (length nums)]
               [indexed-nums (indexed nums)]
               [(list _ max-index*) (argmax car indexed-nums)]
               [(list _ min-index*) (argmin car indexed-nums)]
               [max-index (add1 max-index*)]
               [min-index (add1 min-index*)]
               [right-max-index (- len max-index*)]
               [right-min-index (- len min-index*)])
    (min
      (max max-index min-index)
      (max right-max-index right-min-index)
      (min (+ max-index right-min-index) (+ min-index right-max-index)))))
