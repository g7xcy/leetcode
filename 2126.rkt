(define/contract (asteroids-destroyed mass asteroids)
  (-> exact-integer? (listof exact-integer?) boolean?)
  (let ([soretd-asteroids (sort asteroids <)])
    (positive?
      (foldl
        (lambda (x acc) (if (>= acc x) (+ acc x) 0))
        mass
        soretd-asteroids))))
