;; accumulate-costs :: Integer -> Pair Integer Integer -> List Integer
(define ((accumulate-costs best-end) cost acc)
  (match cost
    [(list start duration) (cons (+ (max best-end start) duration) acc)]))

;; best-one->another :: Integer -> List (Integer Integer) -> Integer
(define (best-one->another best-one-end another-costs)
  (apply min
         (foldl
           (accumulate-costs best-one-end)
           '()
           another-costs)))

(define/contract (earliest-finish-time landStartTime landDuration waterStartTime waterDuration)
  (-> (listof exact-integer?)
      (listof exact-integer?)
      (listof exact-integer?)
      (listof exact-integer?)
      exact-integer?)
  (let* ([lands (map list landStartTime landDuration)]
         [waters (map list waterStartTime waterDuration)]
         [best-land (apply min (map (curry apply +) lands))]
         [best-water (apply min (map (curry apply +) waters))])
    (min (best-one->another best-land waters)
         (best-one->another best-water lands))))
