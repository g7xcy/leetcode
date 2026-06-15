;; safe-drop-last :: List a -> List a
(define (safe-drop-last xs)
  (match xs
    ['() '()]
    [_ (drop-right xs 1)]))

;; step :: Char -> List Char -> List Char
(define (step ch result)
  (match ch
    [#\* (safe-drop-last result)]
    [#\# (append result result)]
    [#\% (reverse result)]
    [else (append result (list ch))]))

(define/contract (process-str s)
  (-> string? string?)
  (list->string
    (foldl step '() (string->list s))))
