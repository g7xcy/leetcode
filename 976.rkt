(define (get-largest-perimeter nums)
  (if (null? (cddr nums))
      0
      (if (> (+ (cadr nums) (caddr nums)) (car nums))
          (+ (car nums) (cadr nums) (caddr nums))
          (get-largest-perimeter (cdr nums)))))

(define/contract (largest-perimeter nums)
  (-> (listof exact-integer?) exact-integer?)
  (get-largest-perimeter (sort nums >)))
