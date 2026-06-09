(define (zipped-romans s)
  (let* ([xs (map string (string->list s))]
         [ys (append (cdr xs) '(""))])
    (foldr (lambda (x y acc) (cons (string-append x y) acc)) '() xs ys)))

(define (safe-cdr xs)
  (cond
    [(null? xs) '()]
    [else (cdr xs)]))

(define (safe-cddr xs)
  (cond
    [(null? xs) '()]
    [else (safe-cdr (cdr xs))]))

(define/contract (roman-to-int s)
  (-> string? exact-integer?)
  (letrec ([solve
            (lambda (xs)
              (if (null? xs)
                  0
                  (let ([x (car xs)])
                    (cond
                      [(string=? x "IV") (+ 4 (solve (safe-cddr xs)))]
                      [(string=? x "IX") (+ 9 (solve (safe-cddr xs)))]
                      [(string=? x "XL") (+ 40 (solve (safe-cddr xs)))]
                      [(string=? x "XC") (+ 90 (solve (safe-cddr xs)))]
                      [(string=? x "CD") (+ 400 (solve (safe-cddr xs)))]
                      [(string=? x "CM") (+ 900 (solve (safe-cddr xs)))]
                      [else (+
                              (solve (safe-cdr xs))
                              (let ([c (string-ref x 0)])
                              (cond
                                [(eq? c #\I) 1]
                                [(eq? c #\V) 5]
                                [(eq? c #\X) 10]
                                [(eq? c #\L) 50]
                                [(eq? c #\C) 100]
                                [(eq? c #\D) 500]
                                [(eq? c #\M) 1000]
                                [else 0])
                              ))]))))])
    (solve (zipped-romans s))))