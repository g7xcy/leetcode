;; split-continuous-zeros :: [Integer] -> [Integer]
(define (split-continuous-zeros xs)
  (if (null? xs)
      '()
      (call-with-values
        (lambda () (splitf-at xs zero?))
        (lambda (continuous-zeros not-continuous-zeros)
          (cons (length continuous-zeros) (split-continuous-zeros (dropf not-continuous-zeros (compose not zero?))))))))

(define/contract (zero-filled-subarray nums)
  (-> (listof exact-integer?) exact-integer?)
  (apply +
         (map (lambda (len) (/ (* len (add1 len)) 2))
              (split-continuous-zeros nums))))
