(define/contract (maximum-element-after-decrementing-and-rearranging arr)
  (-> (listof exact-integer?) exact-integer?)
  (foldl
    (lambda (x best) (min x (add1 best)))
    0
    (sort arr <)))
