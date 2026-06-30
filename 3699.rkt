(define MOD (+ (expt 10 9) 7))

;; prefix-exclusive :: Vector Integer -> Vector Integer
;; result[x] = vec[0] + ... + vec[x - 1]
(define (prefix-exclusive vec)
  (let* ([m (vector-length vec)]
         [res (make-vector m 0)])
    (let loop ([i 0] [sum 0])
      (cond
        [(= i m) res]
        [else
         (vector-set! res i sum)
         (loop (add1 i)
               (modulo (+ sum (vector-ref vec i)) MOD))]))))

;; suffix-exclusive :: Vector Integer -> Vector Integer
;; result[x] = vec[x + 1] + ... + vec[m - 1]
(define (suffix-exclusive vec)
  (let* ([m (vector-length vec)]
         [res (make-vector m 0)])
    (let loop ([i (sub1 m)] [sum 0])
      (cond
        [(< i 0) res]
        [else
         (vector-set! res i sum)
         (loop (sub1 i)
               (modulo (+ sum (vector-ref vec i)) MOD))]))))

(define/contract (zig-zag-arrays n l r)
  (-> exact-integer? exact-integer? exact-integer? exact-integer?)
  (let ([m (add1 (- r l))])
    (cond
      [(= n 1) m]
      [(= m 1) 0]
      [else
       (let ([up (build-vector m identity)]
             [down (build-vector m (lambda (x) (- m 1 x)))])
         (let loop ([len 2]
                    [up up]
                    [down down])
           (cond
             [(= len n)
              (modulo
               (+ (for/sum ([x (in-vector up)]) x)
                  (for/sum ([x (in-vector down)]) x))
               MOD)]
             [else
              (loop
               (add1 len)
               (prefix-exclusive down)
               (suffix-exclusive up))])))])))
