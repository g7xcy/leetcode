; ans = (max (+ (rob (- x 2) (rob x))) (rob (- x 1)))
(define/contract (rob nums)
  (-> (listof exact-integer?) exact-integer?)
  (letrec ([loop (lambda (d2 d1 xs)
                   (if (null? xs)
                       d1
                       (loop d1 (max (+ d2 (car xs)) d1) (cdr xs))))])
    (loop 0 0 nums)))