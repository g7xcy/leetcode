(define/contract (rob nums)
  (-> (listof exact-integer?) exact-integer?)
  (letrec ([loop (lambda (d2 d1 xs)
                   (if (null? xs)
                       d1
                       (loop d1 (max (+ d2 (car xs)) d1) (cdr xs))))])
    (max
      (car nums)
      (loop 0 0 (take nums (sub1 (length nums))))
      (loop 0 0 (cdr nums)))))
