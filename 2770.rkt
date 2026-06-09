;; jump? :: Vector Integer -> Integer -> Integer -> Integer -> Boolean
(define (jump? nums target index-i index-j)
  (and (< index-i index-j)
       (<= (abs (-
                  (vector-ref nums index-i)
                  (vector-ref nums index-j)))
           target)))

;; next-indexes :: Vector Integer -> Integer -> Integer -> [Integer]
(define (next-indexes nums target curr)
  (filter (curry jump? nums target curr) (range (add1 curr) (vector-length nums))))

;; memo-search :: Vector Integer -> Hash Integer Integer -> Index
(define (memo-search nums target cache index)
  (cond
    [(= (sub1 (vector-length nums)) index) 0]
    [(hash-has-key? cache index) (hash-ref cache index)]
    [else (let* ([next-indexes (next-indexes nums target index)]
                 [best-ans* (apply max (cons -1 (map (curry memo-search nums target cache) next-indexes)))]
                 [best-ans (if (= -1 best-ans*) -1 (add1 best-ans*))])
            (hash-set! cache index best-ans)
            best-ans)]))

(define/contract (maximum-jumps nums target)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let ([cache (make-hash)]
        [vec-nums (list->vector nums)])
    (memo-search vec-nums target cache 0)))
