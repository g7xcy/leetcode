(define (add-carry xs carry)
  (cond
    [(and (null? xs) (zero? carry)) '()]
    [(null? xs) (list carry)]
    [(zero? carry) xs]
    [else (let ([temp (+ (car xs) carry)])
            (if (>= temp 2)
                (cons (remainder temp 2) (add-carry (cdr xs) 1))
                (cons temp (cdr xs))))]))

(define (add-two-numbers-list xs ys carry)
  (cond
    [(null? xs) (add-carry ys carry)]
    [(null? ys) (add-carry xs carry)]
    [else
     (let ([temp (+ (car xs) (car ys) carry)])
       (cons (remainder temp 2) (add-two-numbers-list (cdr xs) (cdr ys) (quotient temp 2))))]))

(define (string->numbers s)
  (map (lambda (x) (- (char->integer x) (char->integer #\0))) (string->list s)))

(define (numbers->string xs)
  (list->string (map (lambda (x) (integer->char (+ x (char->integer #\0)))) xs)))

(define/contract (add-binary a b)
  (-> string? string? string?)
  (numbers->string
    (reverse
      (add-two-numbers-list
        (reverse (string->numbers a))
        (reverse (string->numbers b))
        0))))
