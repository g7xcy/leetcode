(define (transpose mat)
  (apply map list mat))

(define (cons-if-not-empty xs rest)
  (if (null? xs)
      rest
      (cons xs rest)))

(define (continuous-stone? c)
  (char=? #\# c))
(define (continuous-others? c)
  (not (char=? #\# c)))
(define (continuous-empty? c)
  (char=? #\. c))

(define (split-others row)
  (cond
    [(null? row) '()]
    [(null? (cdr row)) (list row)]
    [else (call-with-values
            (lambda () (splitf-at row continuous-others?))
            (lambda (others rest)
              (cons-if-not-empty others (split-stones rest))))]))

(define (split-stones row)
  (cond
    [(null? row) '()]
    [(null? (cdr row)) (list row)]
    [else (call-with-values
            (lambda () (splitf-at row continuous-stone?))
            (lambda (stones rest)
              (cons-if-not-empty stones (split-others rest))))]))

(define (shift-row row)
  (foldr
    (lambda (x acc)
      (if (not (char=? #\# (car x)))
          (append x acc)
          (call-with-values
            (lambda () (splitf-at acc continuous-empty?))
            (lambda (emptys rest) (append emptys x rest)))))
    '()
    (split-stones row)))

(define (shift-box box)
  (map shift-row box))

(define (rotate-box box)
  (map reverse
       (transpose box)))

(define/contract (rotate-the-box boxGrid)
  (-> (listof (listof char?)) (listof (listof char?)))
  ((compose rotate-box shift-box) boxGrid))
