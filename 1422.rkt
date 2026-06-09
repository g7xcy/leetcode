#lang racket

; current-score = (+ (- (add1 index) current-prefix-1) (- (sum s) current-prefix-1))
(define/contract (max-score s)
  (-> string? exact-integer?)
  (let*
    ([nums (map (lambda (c) (- (char->integer c) (char->integer #\0))) (string->list s))]
     [total-1 (apply + nums)])
    (for/fold
      ([prefix-1 0]
       [ans 0]
       #:result ans)
      ([(x i) (in-indexed (take nums (sub1 (length nums))))])
      (let*
        ([current-prefix-1 (+ prefix-1 x)]
         [max-score (max ans (+ (- (add1 i) current-prefix-1) (- total-1 current-prefix-1)))])
        (values current-prefix-1 max-score)))))
