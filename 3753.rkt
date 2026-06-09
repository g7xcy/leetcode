;; digit-char->integer :: Char -> Integer
(define (digit-char->integer ch)
  (- (char->integer ch)
     (char->integer #\0)))

;; integer->digits :: Integer -> List Integer
(define (integer->digits n)
  (map digit-char->integer
       (string->list (number->string n))))

;; peak? :: Integer -> Integer -> Integer -> Boolean
(define (peak? x y z)
  (and (> y x) (> y z)))

;; valley? :: Integer -> Integer -> Integer -> Boolean
(define (valley? x y z)
  (and (< y x) (< y z)))

;; peak-or-valley? :: Integer -> Integer -> Integer -> Boolean
(define (peak-or-valley? x y z)
  (or (peak? x y z)
      (valley? x y z)))

;; waviness-sum-until :: Integer -> Integer
(define (waviness-sum-until n)
  (if (< n 0)
      0
      (let* ([digits (list->vector (integer->digits n))]
             [len (vector-length digits)]
             [memo (make-hash)])

        ;; dfs :: Integer -> (Integer | #f) -> (Integer | #f) -> Integer -> Boolean -> Pair Integer Integer
        (letrec ([dfs
                  (lambda (pos prev2 prev1 used tight)
                    (let* ([key (list pos prev2 prev1 used tight)]
                           [cached (hash-ref memo key #f)])
                      (if cached
                          cached
                          (let ([result
                                 (if (= pos len)
                                     (cons 1 0)
                                     (let* ([limit (if tight (vector-ref digits pos) 9)])
                                       (let loop ([d 0] [total-count 0] [total-sum 0])
                                         (if (> d limit)
                                             (cons total-count total-sum)
                                             (let* ([next-tight (and tight (= d limit))]
                                                    [child
                                                     (cond
                                                       [(and (= used 0) (= d 0)) (dfs (add1 pos) #f #f 0 next-tight)]
                                                       [(= used 0) (dfs (add1 pos) #f d 1 next-tight)]
                                                       [(= used 1) (dfs (add1 pos) prev1 d 2 next-tight)]
                                                       [else
                                                        (let* ([extra (if (peak-or-valley? prev2 prev1 d) 1 0)]
                                                               [sub (dfs (add1 pos) prev1 d 2 next-tight)]
                                                               [sub-count (car sub)]
                                                               [sub-sum (cdr sub)])
                                                          (cons sub-count (+ sub-sum (* extra sub-count))))])])
                                               (loop (add1 d)
                                                     (+ total-count (car child))
                                                     (+ total-sum (cdr child))))))))])
                            (hash-set! memo key result)
                            result))))])
          (cdr (dfs 0 #f #f 0 #t))))))

(define/contract (total-waviness num1 num2)
  (-> exact-integer? exact-integer? exact-integer?)
  (- (waviness-sum-until num2)
     (waviness-sum-until (sub1 num1))))
