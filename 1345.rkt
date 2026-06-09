(require data/queue)

;; at :: Vector Integer -> Integer -> Either Integer Boolean
(define ((at vec) index)
  (if (and (< index (vector-length vec))
           (>= index 0))
      (vector-ref vec index)
      #f))

(define ((update-mut-set index) st)
  (begin
    (set-add! st index)
    st))

;; update-mut-hash :: Hash Integer (Set Integer) -> Integer -> (Set Integer -> Integer -> Set Integer) -> Set Integer -> Hash Integer (Set Integer)
(define (update-mut-hash ht key f default)
  (begin
    (hash-update! ht key f default)
    ht))

;; update-mut-queue :: Queue Integer -> Set Integer -> Queue Integer
(define (update-mut-queue queue vals)
  (begin
    (for ([val vals])
      (enqueue! queue val))
    queue))

;; buckets :: Vector Integer -> Hash Integer Integer -> Hash Integer (Set Integer)
(define (buckets at bucket index)
  (let ([val (at index)])
    (if (not val)
        bucket
        (buckets
          at
          (update-mut-hash bucket val (update-mut-set index) (mutable-set index))
          (add1 index)))))

;; next :: (Integer -> Integer) -> Hash Integer (Set Integer) -> Integer -> Set Integer
(define ((next at buckets) index)
  (let ([val (at index)])
    (if (not val)
        (mutable-set)
        (begin
          (let ([st (hash-ref buckets (at index) (mutable-set))])
            (hash-remove! buckets (at index))
            (set-add! st (sub1 index))
            (set-add! st (add1 index))
            st)))))

;; bfs :: (Integer -> Set Integer) -> Integer -> Queue Integer -> Set Integer -> Integer
(define (bfs next target queue visited)
  (match-let ([(list index step) (dequeue! queue)])
    (cond
      [(= index target) step]
      [else
       (for ([nxt (in-set (next index))]
             #:when (not (set-member? visited nxt)))
         (set-add! visited nxt)
         (enqueue! queue (list nxt (add1 step))))
       (bfs next target queue visited)])))

(define/contract (min-jumps arr)
  (-> (listof exact-integer?) exact-integer?)
  (let* ([vec (list->vector arr)]
         [at (at vec)]
         [buckets (buckets at (make-hash) 0)]
         [next (next at buckets)]
         [target (sub1 (vector-length vec))]
         [visited (mutable-set)]
         [queue (make-queue)])
    (enqueue! queue (list 0 0))
    (bfs next target queue visited)))