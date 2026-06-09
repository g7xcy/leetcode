(define (on-the-line? k b coordinate)
  (= (+ (* k (car coordinate)) b) (cadr coordinate)))

(define/contract (check-straight-line coordinates)
  (-> (listof (listof exact-integer?)) boolean?)
  (let*
    ([x1 (caar coordinates)]
     [y1 (cadar coordinates)]
     [x2 (caadr coordinates)]
     [y2 (cadadr coordinates)])
    (if (= x1 x2)
        (andmap (lambda (p) (= x1 (car p))) (drop coordinates 2))
        (let*
          ([k (/ (- y1 y2) (- x1 x2))]
           [b (- y1 (* k x1))])
          (andmap (curry on-the-line? k b) (drop coordinates 2))))))
