(require math/number-theory)
(require data/queue)

;; update-bucket :: Hash [Integer, Set Integer] -> [Integer] -> Integer -> Hash [Integer, Set Integer]
(define (update-bucket bucket factors index)
  (foldl
    (lambda (factor acc) (hash-update acc factor (lambda (s) (set-add s index)) (set)))
    bucket
    factors))

;; build-bucket :: Vector Integer -> Hash [Integer, Set Integer] -> Integer -> Hash [Integer, Set Integer]
(define (build-bucket nums bucket index)
  (let ([len (vector-length nums)])
    (if (< index len)
        (build-bucket
          nums
          (update-bucket
            bucket
            (map car (factorize (vector-ref nums index)))
            index)
          (add1 index))
        bucket)))

;; neighbours :: Vector Integer -> Hash [Integer, Set Integer] -> Integer -> Values (Set Integer) (Set Integer)
;; Precondition: 0 <= index < (vector-length nums)

(define (neighbours nums bucket used-primes index)
  (let*
    ([v (vector-ref nums index)]
     [len (vector-length nums)]
     [left (sub1 index)]
     [right (add1 index)]
     [direct-move (list->set (filter (lambda (x) (and (>= x 0) (< x len))) (list left right)))]
     [should-expand-prime? (and (prime? v) (not (set-member? used-primes v)))]
     [tp-move
      (if should-expand-prime?
          (set-remove (hash-ref bucket v (set)) index)
          (set))]
     [new-used-primes
      (if should-expand-prime?
          (set-add used-primes v)
          used-primes)])
    (values (set-union direct-move tp-move) new-used-primes)))

;; enqueue-all! :: Queue [Integer Integer] -> [Integer Integer] -> Queue [Integer Integer]
(define (enqueue-all! queue xs)
  (begin
    (for ([x xs]) (enqueue! queue x))
    queue))

;; bfs :: Vector Integer -> Hash [Integer, Set Integer] -> Queue [Integer Integer] -> Set Integer -> Set Integer -> Integer
(define (bfs nums bucket queue visited used-primes)
  (let* ([p (dequeue! queue)]
         [index (car p)]
         [depth (cadr p)]
         [end (sub1 (vector-length nums))])
    (if (= index end)
        depth
        (let-values ([(all-nexts new-used-primes) (neighbours nums bucket used-primes index)])
          (let* ([nexts (set-subtract all-nexts visited)]
                 [new-visited (set-union visited nexts)]
                 [next-states (set-map nexts (lambda (x) (list x (add1 depth))))])
            (bfs nums
                 bucket
                 (enqueue-all! queue next-states)
                 new-visited
                 new-used-primes))))))

(define/contract (min-jumps nums)
  (-> (listof exact-integer?) exact-integer?)
  (let* ([vec (list->vector nums)]
         [bucket (build-bucket vec (hash) 0)]
         [queue (make-queue)])
    (enqueue! queue (list 0 0))
    (bfs vec bucket queue (set 0) (set))))
