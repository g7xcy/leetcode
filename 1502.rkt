(define/contract (can-make-arithmetic-progression arr)
  (-> (listof exact-integer?) boolean?)
  (let* ([sorted (sort arr <)]
         [q (- (car sorted) (cadr sorted))])
    (letrec
      ([ans (lambda (xs prev)
              (if (null? xs)
                  #t
                  (and
                    (= q (- prev (car xs)))
                    (ans (cdr xs) (car xs)))))])
      (ans (cdr sorted) (car sorted)))))
