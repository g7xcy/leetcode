(define/contract (average salary)
  (-> (listof exact-integer?) flonum?)
  (let ([l (- (length salary) 2)])
   (exact->inexact (/ (apply + (take (cdr (sort salary <)) l)) l))))
