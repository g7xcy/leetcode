(define (string->numbers s)
  (map (lambda (c) (- (char->integer c) (char->integer #\0))) (string->list s)))

(define (get-sign s)
  (if (zero? (string-length s))
      +
      (case (string-ref s 0)
        [(#\-) -]
        [else +])))

(define (drop-sign s)
  (if (zero? (string-length s))
      ""
      (if (or (eq? #\- (string-ref s 0)) (eq? #\+ (string-ref s 0)))
          (substring s 1)
          s)))

(define (keep-digits xs)
  (let-values
    ([(result _) (splitf-at xs (lambda (x) (and (>= x 0) (<= x 9))))])
    result))

(define (skip-zeros xs)
  (let-values
    ([(_ result) (splitf-at xs zero?)])
    result))

(define/contract (my-atoi s)
  (-> string? exact-integer?)
  (let*
    ([trimmed-s (string-trim s)]
     [sign (get-sign trimmed-s)]
     [xs (skip-zeros (keep-digits (string->numbers (drop-sign trimmed-s))))]
     [num (sign (foldl (lambda (x acc) (+ x (* acc 10))) 0 xs))])
    (cond
      [(> num 2147483647) 2147483647]
      [(< num -2147483648) -2147483648]
      [else num])))
