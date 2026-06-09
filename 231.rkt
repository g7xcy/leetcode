(define/contract (is-power-of-two n)
  (-> exact-integer? boolean?)
  (and
    (> 0 n)
    (= 0 (bitwise-and n (- n 1)))))
