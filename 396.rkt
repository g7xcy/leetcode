;; F(k) = F(k - 1) + S - n * nums[n - k]

;; tail rec
(define (fs vec prev s k n acc)
  (if (= k n)
      acc
      (let ([cur (+ prev s (- (* n (vector-ref vec (- n k)))))])
        (fs vec cur s (add1 k) n (cons cur acc)))))

(define/contract (max-rotate-function nums)
  (-> (listof exact-integer?) exact-integer?)
  (let* ([vec (list->vector nums)]
         [n (length nums)]
         [s (apply + nums)]
         [f0 (for/sum ([(x i) (in-indexed nums)])
               (* x i))])
    (apply max (fs vec f0 s 1 n (list f0)))))
