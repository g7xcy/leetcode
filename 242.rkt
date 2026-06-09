(define/contract (is-anagram s t)
  (-> string? string? boolean?)
  (andmap
    zero?
    (hash-values
      (foldr
        (lambda (x acc) (hash-update acc x sub1 -1))
        (foldr (lambda (x acc) (hash-update acc x add1 0)) (hash) (string->list s))
        (string->list t)))))
