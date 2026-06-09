;; all-lowercase-letters :: List Char
(define all-lowercase-letters
  (map integer->char
       (inclusive-range (char->integer #\a) (char->integer #\z))))

;; count-special-chars :: Set Char -> List Char -> Integer
(define (count-special-chars st keys)
  (cond
    [(null? keys) 0]
    [(and (set-member? st (car keys))
          (set-member? st (char-upcase (car keys))))
     (add1 (count-special-chars st (cdr keys)))]
    [else
     (count-special-chars st (cdr keys))]))

;; string->set :: String -> Set Char
(define string->set (compose list->set string->list))

(define/contract (number-of-special-chars word)
  (-> string? exact-integer?)
  (let ([st (string->set word)])
    (count-special-chars st all-lowercase-letters)))
