(define/contract (find-max-consecutive-ones nums)
  (-> (listof exact-integer?) exact-integer?)
  (if (null? nums)
      0
      (call-with-values
        (thunk (splitf-at nums (curry = 1)))
        (lambda (xs ys)
          (max
            (length xs)
            (find-max-consecutive-ones (dropf ys zero?)))))))
