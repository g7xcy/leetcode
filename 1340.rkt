;; at :: Vector Integer -> Integer -> Integer | False
(define ((at vec) index)
  (cond
    [(< index 0) #f]
    [(>= index (vector-length vec)) #f]
    [else (vector-ref vec index)]))

;; lower-than? :: (Integer -> Integer | False) -> Integer -> Integer -> Boolean
(define ((lower-than? at target) index)
  (let ([val (at index)])
    (and val (< val target))))

;; scan-one-side :: (Integer -> Integer | False) -> Integer -> List Integer -> List Integer
(define (scan-one-side at target candidates)
  (takef candidates
         (lower-than? at target)))

;; scan :: (Integer -> Integer | False) -> Integer -> Integer -> Integer -> List Integer
(define (scan at index target d)
  (let ([left-candidates (inclusive-range (sub1 index) (- index d) -1)]
        [right-candidates (inclusive-range (add1 index) (+ index d))])
    (append
      (scan-one-side at target left-candidates)
      (scan-one-side at target right-candidates))))

;; dp(i) = max(dp(i-m), ... dp(i+1), ..., dp(i+n)) + 1
;; search :: (Integer -> Integer | False) -> Integer -> MutHash -> Integer -> Integer
(define ((search at d memo) index)
  (cond
    [(not (at index)) 0]
    [(hash-has-key? memo index) (hash-ref memo index)]
    [else
     (let* ([curr (at index)]
            [nexts (scan at index curr d)]
            [best (add1
                    (apply max
                      (cons 0 (map (search at d memo) nexts))))])
       (hash-set! memo index best)
       best)]))

(define/contract (max-jumps arr d)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let* ([vec (list->vector arr)]
         [at (at vec)]
         [memo (make-hash)]
         [search (search at d memo)])
    (apply max
           (map search
                (range 0 (vector-length vec))))))
