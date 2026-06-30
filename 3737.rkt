(struct segment-tree (raw len make-leaf merge)
  #:transparent)

;; make-segment-tree :: Vectorof A -> (A -> Node) -> (Node -> Node -> Node) -> SegmentTree
(define (make-segment-tree vec make-leaf merge)
  (if (vector-empty? vec)
      (error 'make-segment-tree "empty vector")
      (let* ([vec-len (vector-length vec)]
             [st-len (* 4 vec-len)]
             [raw (make-vector st-len #f)]
             [build! (build! make-leaf merge vec raw)])
        (build! 1 0 (sub1 vec-len))
        (segment-tree raw vec-len make-leaf merge))))

;; build! :: (A -> Node)
;;        -> (Node -> Node -> Node)
;;        -> Vectorof A
;;        -> Vectorof (U Node #f)
;;        -> Integer
;;        -> Integer
;;        -> Integer
;;        -> Node
(define ((build! make-leaf merge vec raw) pos left right)
  (let build-rec ([pos pos]
                  [left left]
                  [right right])
    (if (= left right)
        (let ([node (make-leaf (vector-ref vec left))])
          (vector-set! raw pos node)
          node)
        (let* ([mid (quotient (+ left right) 2)]
               [left-child-pos (* 2 pos)]
               [right-child-pos (add1 left-child-pos)]
               [left-child-node (build-rec left-child-pos left mid)]
               [right-child-node (build-rec right-child-pos (add1 mid) right)]
               [current-node (merge left-child-node right-child-node)])
          (vector-set! raw pos current-node)
          current-node))))

;; segment-tree-query :: SegmentTree -> Integer -> Integer -> Node
(define (segment-tree-query st q-left q-right)
  (let ([len (segment-tree-len st)])
    (if (not (and
               (<= q-left q-right)
               (>= q-left 0)
               (< q-right len)))
        (error 'segment-tree-query "invalid query range")
        (query-rec st 1 0 (sub1 len) q-left q-right))))

;; query-rec :: SegmentTree
;;           -> Integer
;;           -> Integer
;;           -> Integer
;;           -> Integer
;;           -> Integer
;;           -> Node
(define (query-rec st pos left right q-left q-right)
  (let* ([merge (segment-tree-merge st)]
         [raw (segment-tree-raw st)])
    (if (and (<= q-left left) (<= right q-right))
        (vector-ref raw pos)
        (let* ([mid (quotient (+ left right) 2)]
               [left-child-pos (* 2 pos)]
               [right-child-pos (add1 left-child-pos)])
          (cond
            [(<= q-right mid) (query-rec st left-child-pos left mid q-left q-right)]
            [(< mid q-left) (query-rec st right-child-pos (add1 mid) right q-left q-right)]
            [else (merge
                    (query-rec st left-child-pos left mid q-left mid)
                    (query-rec st right-child-pos (add1 mid) right (add1 mid) q-right))])))))

;; segment-tree-update! :: SegmentTree -> Integer -> A -> Void
(define (segment-tree-update! st index new-val)
  (let ([len (segment-tree-len st)])
    (if (not (and
               (>= index 0)
               (< index len)))
        (error 'segment-tree-update! "invalid index")
        (update-rec! st 1 0 (sub1 len) index new-val))
    (void)))

;; update-rec! :: SegmentTree
;;             -> Integer
;;             -> Integer
;;             -> Integer
;;             -> Integer
;;             -> A
;;             -> Void
(define (update-rec! st pos left right index new-val)
  (let ([raw (segment-tree-raw st)]
        [make-leaf (segment-tree-make-leaf st)]
        [merge (segment-tree-merge st)])
    (if (= left right)
        (vector-set! raw pos (make-leaf new-val))
        (let* ([mid (quotient (+ left right) 2)]
               [left-child-pos (* 2 pos)]
               [right-child-pos (add1 left-child-pos)])
          (if (<= index mid)
              (update-rec! st left-child-pos left mid index new-val)
              (update-rec! st right-child-pos (add1 mid) right index new-val))
          (vector-set! raw pos
                       (merge (vector-ref raw left-child-pos)
                              (vector-ref raw right-child-pos)))))))

;; segment-tree-update-with! :: SegmentTree -> Integer -> (Node -> Node) -> Void
(define (segment-tree-update-with! st index f)
  (segment-tree-update! st index (f (segment-tree-query st index index))))

;; count-less-than :: SegmentTree -> Integer -> Integer
(define (count-less-than st idx)
  (if (zero? idx)
      0
      (segment-tree-query st 0 (sub1 idx))))

(define/contract (count-majority-subarrays nums target)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let* ([xs (map (lambda (x) (if (= x target) 1 -1)) nums)]
         [len (length xs)]
         [offset len]
         [value-len (add1 (* 2 len))]
         [st (make-segment-tree (make-vector value-len 0) identity +)])
    (let rec ([xs xs]
              [prefix 0]
              [ans 0])
      (let* ([idx (+ prefix offset)]
             [less-count (count-less-than st idx)])
        (segment-tree-update-with! st idx add1)
        (match xs
          ['() (+ ans less-count)]
          [(cons x rest)
           (rec rest
                (+ prefix x)
                (+ ans less-count))])))))
