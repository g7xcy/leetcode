(require data/queue)
(require racket/set)

;; build-spf : Integer -> Vector
;; spf[x] = x 的最小质因子
;; 如果 spf[x] = x，则 x 是质数
(define (build-spf m)
  (define spf (make-vector (add1 m) 0))
  (when (>= m 1)
    (vector-set! spf 1 1))

  (for ([i (in-range 2 (add1 m))])
    (when (= (vector-ref spf i) 0)
      (vector-set! spf i i)
      (when (<= (* i i) m)
        (for ([j (in-range (* i i) (add1 m) i)])
          (when (= (vector-ref spf j) 0)
            (vector-set! spf j i))))))

  spf)

;; prime? by spf
(define (prime-by-spf? spf x)
  (and (>= x 2)
       (= (vector-ref spf x) x)))

;; distinct-prime-factors : Vector Integer -> Integer -> Listof Integer
(define (distinct-prime-factors spf x)
  (let loop ([x x] [acc '()])
    (cond
      [(<= x 1)
       acc]
      [else
       (define p (vector-ref spf x))
       (let remove-same ([y x])
         (if (and (> y 1)
                  (= (vector-ref spf y) p))
             (remove-same (quotient y p))
             (loop y (cons p acc))))])))

;; build-bucket : Vectorof Integer -> Vector -> Hash Integer (Listof Integer)
(define (build-bucket nums spf)
  (define n (vector-length nums))
  (define bucket (make-hash))

  (for ([i (in-range n)])
    (define x (vector-ref nums i))
    (for ([p (in-list (distinct-prime-factors spf x))])
      (hash-update! bucket
                    p
                    (lambda (xs) (cons i xs))
                    '())))

  bucket)

;; bfs : Vectorof Integer -> Vector -> Hash Integer (Listof Integer) -> Integer
(define (bfs nums spf bucket)
  (define n (vector-length nums))
  (define end (sub1 n))
  (define queue (make-queue))
  (define visited (make-vector n #f))
  (define used-primes (mutable-set))

  (enqueue! queue (cons 0 0))
  (vector-set! visited 0 #t)

  (let loop ()
    (let* ([state (dequeue! queue)]
           [index (car state)]
           [depth (cdr state)]
           [next-depth (add1 depth)])
      (if (= index end)
          depth
          (let ([left (sub1 index)]
                [right (add1 index)]
                [v (vector-ref nums index)])
            ;; left
            (when (and (>= left 0)
                       (not (vector-ref visited left)))
              (vector-set! visited left #t)
              (enqueue! queue (cons left next-depth)))

            ;; right
            (when (and (< right n)
                       (not (vector-ref visited right)))
              (vector-set! visited right #t)
              (enqueue! queue (cons right next-depth)))

            ;; teleport
            (when (and (prime-by-spf? spf v)
                       (not (set-member? used-primes v)))
              (set-add! used-primes v)
              (for ([next (in-list (hash-ref bucket v '()))])
                (when (and (not (= next index))
                           (not (vector-ref visited next)))
                  (vector-set! visited next #t)
                  (enqueue! queue (cons next next-depth)))))

            (loop))))))

(define (min-jumps nums)
  (define vec (list->vector nums))
  (define n (vector-length vec))

  (if (= n 1)
      0
      (let* ([max-v (apply max nums)]
             [spf (build-spf max-v)]
             [bucket (build-bucket vec spf)])
        (bfs vec spf bucket))))