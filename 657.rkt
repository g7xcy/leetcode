(define/contract (judge-circle moves)
  (-> string? boolean?)
  (equal?
    (cons 0 0)
    ((foldr
       (lambda (x acc)
         (case x
           [(#\R) (compose (lambda (p) (cons (car p) (add1 (cdr p)))) acc)]
           [(#\L) (compose (lambda (p) (cons (car p) (sub1 (cdr p)))) acc)]
           [(#\U) (compose (lambda (p) (cons (sub1 (car p)) (cdr p))) acc)]
           [(#\D) (compose (lambda (p) (cons (add1 (car p)) (cdr p))) acc)]))
       identity
       (string->list moves))
     (cons 0 0))))