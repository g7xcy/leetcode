;; all-uppercase-letters :: List Char
(define all-uppercase-letters
  (map integer->char
       (inclusive-range
         (char->integer #\A)
         (char->integer #\Z))))

;; no-upper-seen? :: Hash Char Boolean -> Char -> Boolean
(define (no-upper-seen? ht ch)
  (not (hash-has-key? ht (char-upcase ch))))

;; put-into :: Char -> Hash Char Boolean -> Hash Char Boolean
(define (put-into ch ht)
  (cond
    [(and (char-lower-case? ch) (no-upper-seen? ht ch)) (hash-set ht ch #t)]
    [(char-lower-case? ch) (hash-set ht ch #f)]
    [else (hash-set ht ch #t)]))

;; count-special-chars :: Hash Char Boolean -> List Char -> Integer
(define (count-special-chars ht keys)
  (match keys
    ['() 0]
    [(cons key rest)
     (if (and (hash-ref ht (char-downcase key) #f) (hash-ref ht key #f))
         (add1 (count-special-chars ht rest))
         (count-special-chars ht rest))]))

(define/contract (number-of-special-chars word)
  (-> string? exact-integer?)
  (let* ([xs (string->list word)]
         [ht (foldl put-into (hash) xs)])
    (count-special-chars ht all-uppercase-letters)))
