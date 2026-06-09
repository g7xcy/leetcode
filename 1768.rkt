; Resolution1
(define/contract (merge-alternately word1 word2)
  (-> string? string? string?)
  (list->string
    (letrec ([merge-alternately*
              (lambda (xs ys)
                (cond
                  [(null? xs) ys]
                  [(null? ys) xs]
                  [else (cons (car xs) (cons (car ys) (merge-alternately* (cdr xs) (cdr ys))))]))])
      (merge-alternately* (string->list word1) (string->list word2)))))

; Resolution2
(define/contract (merge-alternately word1 word2)
  (-> string? string? string?)
  (list->string
    (let*
      ([xs (string->list word1)]
       [ys (string->list word2)]
       [len (min (length xs) (length ys))])
      (append
        (foldr (lambda (x y acc) (cons x (cons y acc)))
               '()
               (take xs len)
               (take ys len))
        (drop xs len)
        (drop ys len)))))