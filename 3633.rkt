(define/contract (earliest-finish-time landStartTime landDuration waterStartTime waterDuration)
  (-> (listof exact-integer?)
      (listof exact-integer?)
      (listof exact-integer?)
      (listof exact-integer?)
      exact-integer?)
  (let ([lands (map list landStartTime landDuration)]
        [waters (map list waterStartTime waterDuration)])
    (apply min
           (for*/list ([land lands]
                       [water waters])
             (match-let ([(list ls ld) land]
                         [(list ws wd) water])
               (min
                 (+ (max (+ ls ld) ws) wd)
                 (+ (max (+ ws wd) ls) ld)))))))
