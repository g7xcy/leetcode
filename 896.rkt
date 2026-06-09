(define (is-monotonic-with nums f)
  (if (or (null? nums) (null? (cdr nums)))
      #t
      (and
        (f (car nums) (cadr nums))
        (is-monotonic-with (cdr nums) f))))

(define/contract (is-monotonic nums)
  (-> (listof exact-integer?) boolean?)
  (cond
    [(or (null? nums) (null? (cdr nums))) #t]
    [(> (car nums) (cadr nums)) (is-monotonic-with (cdr nums) >=)]
    [(< (car nums) (cadr nums)) (is-monotonic-with (cdr nums) <=)]
    [else (is-monotonic (cdr nums))]))
