;; find-balloon-letters :: List Char -> Hash Char Integer
(define (find-balloon-letters text)
  (foldl
    (lambda (x acc) (if (hash-has-key? acc x) (hash-update acc x add1) acc))
    (hash #\b 0 #\a 0 #\l 0 #\o 0 #\n 0)
    text))

;; find-balloon-words :: Hash Char Integer -> Integer
(define (find-balloon-words letters)
  (min (hash-ref letters #\b 0)
       (hash-ref letters #\a 0)
       (hash-ref letters #\n 0)
       (quotient (hash-ref letters #\o 0) 2)
       (quotient (hash-ref letters #\l 0) 2)))

(define/contract (max-number-of-balloons text)
  (-> string? exact-integer?)
  (let* ([text-list (string->list text)]
         [balloon-letters (find-balloon-letters text-list)])
    (find-balloon-words balloon-letters)))
