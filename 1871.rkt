;; prefix-at :: Vector Integer -> Integer -> Integer
(define (prefix-at pre index)
  (if (< index 0)
      0
      (vector-ref pre index)))

;; reachable-before? :: Vector Integer -> Integer -> Integer -> Integer -> Boolean
(define (reachable-before? pre i minJump maxJump)
  (let* ([left (max 0 (- i maxJump))]
         [right (- i minJump)])
    (and (>= right 0)
         (positive?
           (- (prefix-at pre right)
              (prefix-at pre (sub1 left)))))))

(define/contract (can-reach s minJump maxJump)
  (-> string? exact-integer? exact-integer? boolean?)
  (let* ([n (string-length s)]
         [f (make-vector n #f)]
         [pre (make-vector n 0)])
    (vector-set! f 0 #t)
    (vector-set! pre 0 1)
    (let loop ([i 1])
      (cond
        [(= i n)
         (vector-ref f (sub1 n))]
        [else
         (when (and (char=? (string-ref s i) #\0)
                    (reachable-before? pre i minJump maxJump))
           (vector-set! f i #t))
         (vector-set! pre i
                      (+ (vector-ref pre (sub1 i))
                         (if (vector-ref f i) 1 0)))
         (loop (add1 i))]))))
