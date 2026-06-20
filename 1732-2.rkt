(define/contract (largest-altitude gain)
  (-> (listof exact-integer?) exact-integer?)
  (cadr
    (foldl
      (lambda (x acc)
        (match-let ([(list altitude best) acc])
          (let ([new-altitude (+ x altitude)])
            (list new-altitude (max best new-altitude)))))
      '(0 0)
      gain)))
