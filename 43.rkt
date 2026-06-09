(define (string->numbers s)
  (map
    (lambda (c) (- (char->integer c) (char->integer #\0)))
    (string->list s)))

(define (numbers->string numbers)
  (list->string
    (map
      (lambda (x) (integer->char (+ x (char->integer #\0))))
      numbers)))

(define (add xs x)
  (if (null? xs)
      (if (zero? x)
          '()
          (list x))
      (let ([temp (+ (car xs) x)])
        (cons (remainder temp 10) (add (cdr xs) (quotient temp 10))))))

(define (add-two-lists xs ys carry)
  (cond
    [(null? xs) (add ys carry)]
    [(null? ys) (add xs carry)]
    [else
     (let ([temp (+ (car xs) (car ys) carry)])
       (cons (remainder temp 10) (add-two-lists (cdr xs) (cdr ys) (quotient temp 10))))]))

(define (multiply-list xs x carry)
  (if (null? xs)
      (if (zero? carry)
          '()
          (list carry))
      (if (zero? x)
          '(0)
          (let ([temp (+ (* (car xs) x) carry)])
            (cons (remainder temp 10) (multiply-list (cdr xs) x (quotient temp 10)))))))

(define (multiply10 xs)
  (cons 0 xs))

(define (multiply-two-lists xs ys)
  (if (null? ys)
      '()
      (add-two-lists
        (multiply-list xs (car ys) 0)
        (multiply10 (multiply-two-lists xs (cdr ys)))
        0)))

(define/contract (multiply num1 num2)
  (-> string? string? string?)
  (if (< (string-length num1) (string-length num2))
      (multiply num2 num1)
      (numbers->string
        (reverse
          (multiply-two-lists
            (reverse (string->numbers num1))
            (reverse (string->numbers num2)))))))
