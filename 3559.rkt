(define BIGINT (+ (expt 10 9) 7))

;; build-info-map :: [Edge] -> Hash Integer (List Integer Integer)
;; Edge => (list Integer Integer)
;; => (parent depth)
(define (build-info-map edges)
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
  ;; info-map
  (let ([edges-map (edges->edges-map edges)])
    (let dfs ([node 1]
              [parent 0]
              [depth 0]
              [info-map (hash)])
      (let ([info-map* (hash-set info-map node (list parent depth))])
        (for/fold ([acc info-map*])
                  ([next (in-set (hash-ref edges-map node (set)))]
                   #:unless (= parent next))
          (dfs next node (add1 depth) acc))))))

;; make-up-table :: Integer -> Hash Integer (List Integer Integer) -> Vectorof Vector
(define (make-up-table n info-map)
  (let* ([logn (add1 (integer-length n))]
         [up (build-vector logn
                           (lambda (_)
                             (make-vector (add1 n) 0)))])
    ;; up[0][node] = parent[node]
    (for ([node (in-range 1 (add1 n))])
      (let ([parent (car (hash-ref info-map node))])
        (vector-set! (vector-ref up 0) node parent)))
    ;; up[j][node] = up[j - 1][up[j - 1][node]]
    (for ([j (in-range 1 logn)])
      (let ([prev (vector-ref up (sub1 j))]
            [curr (vector-ref up j)])
        (for ([node (in-range 1 (add1 n))])
          (let ([mid (vector-ref prev node)])
            (vector-set! curr node
                         (if (zero? mid)
                             0
                             (vector-ref prev mid)))))))
    up))

;; jump-node :: Vectorof Vector -> Integer -> Integer -> Integer
(define (jump-node up node distance)
  (for/fold ([node node])
            ([j (in-range (vector-length up))])
    (if (bitwise-bit-set? distance j)
        (vector-ref (vector-ref up j) node)
        node)))

;; make-lca :: Hash Integer (List Integer Integer) -> Integer -> (Integer -> Integer -> Integer)
(define (make-lca info-map n)
  (define (depth-of node) (cadr (hash-ref info-map node)))
  (define ((ancestor-at up) j node) (vector-ref (vector-ref up j) node))
  (let* ([up (make-up-table n info-map)]
         [logn (vector-length up)]
         [ancestor-at (ancestor-at up)])
    (lambda (node1 node2)
      (let*-values ([(u0 v0)
                     (if (< (depth-of node1) (depth-of node2))
                         (values node2 node1)
                         (values node1 node2))]
                    [(u) (jump-node up u0 (- (depth-of u0) (depth-of v0)))]
                    [(v) v0])
        (cond
          [(= u v) u]
          [else
           (let-values ([(u* v*)
                         (for/fold ([u u] [v v])
                                   ([j (in-range (sub1 logn) -1 -1)])
                           (let ([pu (ancestor-at j u)]
                                 [pv (ancestor-at j v)])
                             (if (not (= pu pv))
                                 (values pu pv)
                                 (values u v))))])
             (ancestor-at 0 u*))])))))

;; make-pow2-table :: Integer -> Vector Integer
(define (make-pow2-table n)
  (list->vector
    (reverse
      (foldl
        (lambda (_ acc) (cons (modulo (* 2 (car acc)) BIGINT) acc))
        '(1)
        (range n)))))

(define/contract (assign-edge-weights edges queries)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)) (listof exact-integer?))
  (let* ([info-map (build-info-map edges)]
         [edge-count (length edges)]
         [node-count (add1 edge-count)]
         [find-lca (make-lca info-map node-count)]
         [table (make-pow2-table edge-count)])
    ;; node-depth :: Integer -> Integer
    (define (node-depth node)
      (cadr (hash-ref info-map node)))
    ;; path-edge-count :: Integer Integer -> Integer
    (define (path-edge-count u v)
      (let ([ancestor (find-lca u v)])
        (- (+ (node-depth u)
              (node-depth v))
           (* 2 (node-depth ancestor)))))
    ;; query-answer :: Query -> Integer
    (define (query-answer query)
      (match-let ([(list u v) query])
        (let ([edge-count (path-edge-count u v)])
          (if (zero? edge-count)
              0
              (vector-ref table (sub1 edge-count))))))
    (map query-answer queries)))

#|
;; lca :: Hash Integer (List Integer Integer) -> Integer -> Integer -> Integer
(define ((lca info-map) node1 node2)
  ;; align-depth :: Hash Integer (List Integer Integer) -> Integer -> Integer -> (Values Integer Integer)
  (define (align-depth node1 node2)
    (match-let ([(list parent1 depth1) (hash-ref info-map node1)]
                [(list parent2 depth2) (hash-ref info-map node2)])
      (cond
        [(= depth1 depth2) (values node1 node2)]
        [(< depth1 depth2) (align-depth node1 parent2)]
        [else (align-depth parent1 node2)])))
  ;; lca
  (let-values ([(node1* node2*) (align-depth node1 node2)])
    (let find-lca ([u node1*] [v node2*])
      (if (= u v)
          u
          (match-let ([(list parent-u _) (hash-ref info-map u)]
                      [(list parent-v _) (hash-ref info-map v)])
            (find-lca parent-u parent-v))))))
|#