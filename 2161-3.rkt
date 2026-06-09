(define/contract (pivot-array nums pivot)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (apply append
         (foldl
           (lambda (x acc)
             (match-let ([(list lt eq gt) acc])
               (cond
                 [(< x pivot) (list (cons x lt) eq gt)]
                 [(= x pivot) (list lt (cons x eq) gt)]
                 [else (list lt eq (cons x gt))])))
           '(() () ())
           nums)))
