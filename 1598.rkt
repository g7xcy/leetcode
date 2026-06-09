(define/contract (min-operations logs)
  (-> (listof string?) exact-integer?)
  ((foldl (lambda (x acc)
            (case x
              [("./") acc]
              [("../") (compose (lambda (x) (if (zero? x) x (sub1 x))) acc)]
              [else (compose add1 acc)]))
          identity
          logs) 0))
