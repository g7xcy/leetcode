;; gt :: [Integer Integer] -> [Integer Integer] -> Boolean
(define (gt task1 task2)
  (> (- (cadr task1) (car task1))
     (- (cadr task2) (car task2))))

(define/contract (minimum-effort tasks)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (let ([sorted-tasks (sort tasks gt)])
    (car
      (foldl
        (match-lambda*
          [(list (list act-eng min-eng) (list init-eng rem-eng))
           (if (>= rem-eng min-eng)
               (list init-eng (- rem-eng act-eng))
               (list (+ init-eng (- min-eng rem-eng)) (- min-eng act-eng)))])
        '(0 0)
        sorted-tasks))))