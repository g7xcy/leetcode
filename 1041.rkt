(define (change-position direction position)
  (case direction
    [(N) (list (sub1 (car position)) (cadr position))]
    [(E) (list (car position) (add1 (cadr position)))]
    [(S) (list (add1 (car position)) (cadr position))]
    [(W) (list (car position) (sub1 (cadr position)))]))
(define (change-direction direction instruction)
  (case instruction
    [(#\L)
     (case direction
       [(N) 'W]
       [(E) 'N]
       [(S) 'E]
       [(W) 'S])]
    [(#\R)
     (case direction
       [(N) 'E]
       [(E) 'S]
       [(S) 'W]
       [(W) 'N])]))
(define (exec-instruction instruction state)
  (let ([direction (car state)]
        [position (cadr state)])
    (case instruction
      [(#\G) (cons direction (list (change-position direction position)))]
      [else (cons (change-direction direction instruction) (list position))])))

(define/contract (is-robot-bounded instructions)
  (-> string? boolean?)
  (let* ([state (foldl exec-instruction (list 'N '(0 0)) (string->list instructions))]
         [direction (car state)]
         [position (cadr state)])
    (or
      (equal? position '(0 0))
      (not (equal? direction 'N)))))
