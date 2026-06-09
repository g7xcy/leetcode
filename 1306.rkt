;; at :: Vector Integer -> Integer -> Integer
(define ((at vec) index)
  (vector-ref vec index))

;; in? :: Integer -> Integer -> Integer -> Boolean
(define ((in? left right) index)
  (and (>= index left)
       (< index right)))

;; visited? :: Set Integer -> Integer -> Boolean
(define ((visited? visited-set) index)
  (begin
    (let ([t (set-member? visited-set index)])
      (set-add! visited-set index)
      t)))

#|
;; update-visited? :: (Integer -> Boolean) -> Integer -> (Integer -> Boolean)
(define ((update-visited? visited? visited-index) index)
    (if (= visited-index index)
    #t
    (visited? index)))

update-visited? 配合(const #f)初值很优雅，但是超时(
|#

;; (Integer -> Integer) -> (Integer -> Boolean) -> (Integer -> Boolean) -> Integer -> Boolean
(define (dfs at in? visited? index)
  (cond
    [(not (in? index)) #f]
    [(zero? (at index)) #t]
    [(visited? index) #f]
    [else (or
            (dfs at in? visited? (+ index (at index)))
            (dfs at in? visited? (- index (at index))))]))

(define/contract (can-reach arr start)
  (-> (listof exact-integer?) exact-integer? boolean?)
  (let ([arr-vec (list->vector arr)])
    (dfs
      (at arr-vec)
      (in? 0 (vector-length arr-vec))
      (visited? (mutable-set))
      start)))
