;; step :: Char -> Integer -> Integer
(define (step ch len)
  (match ch
    [#\* (max 0 (sub1 len))]
    [#\# (* 2 len)]
    [#\% len]
    [else (add1 len)]))

;; make-history :: List Char -> (Char Integer Integer)
;; => (curr-ch prev-len curr-len)
(define (make-history ch histories)
  (match-let ([(list _ _ curr-len) (car histories)])
    (cons (list ch curr-len (step ch curr-len)) histories)))

;; State = ('search Integer) | ('found Char)
;; mapping-step :: (Char Integer Integer) State -> State
(define (mapping-step history-item state)
  (match state
    [(list 'found ch) state]
    [(list 'search k)
     (match-let ([(list ch prev-len curr-len) history-item])
       (match ch
         [#\. state]
         [#\* (list 'search k)]
         [#\# (list 'search (modulo k prev-len))]
         [#\% (list 'search (- prev-len 1 k))]
         [else
          (if (= k prev-len)
              (list 'found ch)
              (list 'search k))]))]))

(define/contract (process-str s k)
  (-> string? exact-integer? char?)
  (let* ([steps (string->list s)]
         [history (foldl make-history '((#\. 0 0)) steps)]
         [final-len (caddar history)])
    (if
      (or (< k 0) (>= k final-len))
      #\.
      (match (foldl mapping-step (list 'search k) history)
        [(list 'found ch) ch]
        [_ #\.]))))
