(define/contract (peak-index-in-mountain-array arr)
  (-> (listof exact-integer?) exact-integer?)
  (letrec
    ([binary-search
      (lambda (l r)
        (if (= l r)
            l
            (let ([mid (quotient (+ l r) 2)])
              (if
                (> (list-ref arr mid) (list-ref arr (add1 mid))) (binary-search l mid)
                (binary-search (add1 mid) r)))))])
    (binary-search 0 (sub1 (length arr)))))

