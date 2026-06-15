;; safe-drop-last :: List a -> List a
(define (safe-drop-last xs)
  (match xs
    ['() '()]
    [_ (drop-right xs 1)]))

;; operate :: List Char -> List Char -> List Char
(define (operate operations result)
  (match operations
    ['() result]
    [(cons #\* rest) (operate rest (safe-drop-last result))]
    [(cons #\# rest) (operate rest (append result result))]
    [(cons #\% rest) (operate rest (reverse result))]
    [(cons ch rest) (operate rest (append result (list ch)))]))

(define/contract (process-str s)
  (-> string? string?)
  (list->string (operate (string->list s) '())))
  