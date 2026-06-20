(define/contract (angle-clock hour minutes)
  (-> exact-integer? exact-integer? flonum?)
  (let* ([h (+ (* 60 (modulo hour 12)) minutes)]
         [m (* 12 minutes)]
         [d (abs (- h m))])
    (exact->inexact
      (/ (min d (- 720 d)) 2.0))))
