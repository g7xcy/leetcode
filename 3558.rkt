(define BIGINT (+ (expt 10 9) 7))

;; Edge => (list Integer Integer)

;; edges->edges-map :: [Edge] -> Hash Integer (Set Integer)
(define (edges->edges-map edges)
  (foldl
    (lambda (edge ht)
      (match-let ([(list u v) edge])
        (hash-update
          (hash-update ht u (lambda (st) (set-add st v)) (set))
          v
          (lambda (st) (set-add st u))
          (set))))
    (hash)
    edges))

;; edges->visited :: [Edge] -> Vector Boolean
(define (edges->visited edges)
  (let ([node-count (foldl (lambda (x count) (apply max (cons count x))) 0 edges)])
    (make-vector (add1 node-count) #f)))

;; dfs :: Hash Integer (Set Integer) -> Vector Boolean -> Integer
(define (dfs edges visited)
  (let dfs-rec ([node 1] [depth 0])
    (vector-set! visited node #t)
    (let ([neighbors (hash-ref edges node (set))])
      (for/fold ([best depth])
                ([next (in-set neighbors)]
                 #:unless (vector-ref visited next))
        (max best
             (dfs-rec next (add1 depth)))))))

(define/contract (assign-edge-weights edges)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (let* ([edges-map (edges->edges-map edges)]
         [visited (edges->visited edges)]
         [max-depth (dfs edges-map visited)])
    (modulo (expt 2 (sub1 max-depth)) BIGINT)))
