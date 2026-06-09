(define (string->integer s)
  (let ([f
         (lambda (xs)
           (foldl
             (lambda (x acc) (+ (- (char->integer x) 48) (* 10 acc)))
             0
             xs))])
    (if (char=? #\- (string-ref s 0))
        (- (f (cdr (string->list s))))
        (f (string->list s)))))

(define/contract (cal-points operations)
  (-> (listof string?) exact-integer?)
  (apply
    +
    (foldl
      (lambda (x acc)
        (if (string? x)
            (case x
              [("+") (cons (+ (car acc) (cadr acc)) acc)]
              [("D") (cons (* (car acc) 2) acc)]
              [("C") (cdr acc)]
              [else (cons (string->integer x) acc)])
            (cons x acc)))
      '()
      operations)))
