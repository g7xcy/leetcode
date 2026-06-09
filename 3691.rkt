(require data/heap)

;; Node :: (Cons Integer Integer)
;; Node => (max min)

;; at :: Vector Integer -> Integer -> (U Integer False)
(define ((at vec) pos)
  (if (and (< pos (vector-length vec)) (>= pos 0))
      (vector-ref vec pos)
      #f))

;; Vector (U Node False) -> Integer -> Node -> (U Node False)
(define ((st-set! vec) pos node)
  (if (and (< pos (vector-length vec)) (>= pos 0))
      (begin (vector-set! vec pos node)
        node)
      #f))

;; merge-node :: Node -> Node -> Node
(define (merge-node left-node right-node)
  (match-let ([(list l-max l-min) left-node]
              [(list r-max r-min) right-node])
    (list (max l-max r-max) (min l-min r-min))))

;; build-st :: (Integer ->(U Integer False)) -> (Integer -> Node -> (U Node False)) -> Integer -> Integer -> Integer -> Node
(define (build-st at st-set! pos left right)
  (if (= left right)
      (let ([val (at left)])
        (st-set! pos (list val val)))
      (let* ([mid (quotient (+ left right) 2)]
             [left-child-pos (* 2 pos)]
             [right-child-pos (add1 left-child-pos)]
             [left-child (build-st at st-set! left-child-pos left mid)]
             [right-child (build-st at st-set! right-child-pos (add1 mid) right)]
             [current-node (merge-node left-child right-child)])
        (st-set! pos current-node))))

;; make-st :: Vector Integer -> Vector Node
(define (make-st vec)
  (let* ([len (vector-length vec)]
         [st (make-vector (* 4 len) #f)])
    (build-st (at vec) (st-set! st) 1 0 (sub1 len))
    st))

;; query-st :: Vector Node -> Integer -> Integer -> Node
(define ((query-st st n) q-left q-right)
  (let query-rec ([pos 1]
                  [left 0]
                  [right (sub1 n)]
                  [q-left q-left]
                  [q-right q-right])
    (cond
      [(and (<= q-left left) (<= right q-right)) (vector-ref st pos)]
      [else
       (let* ([mid (quotient (+ left right) 2)]
              [left-child-pos (* 2 pos)]
              [right-child-pos (add1 left-child-pos)])
         (cond
           [(<= q-right mid)
            (query-rec left-child-pos
                       left
                       mid
                       q-left
                       q-right)]
           [(> q-left mid)
            (query-rec right-child-pos
                       (add1 mid)
                       right
                       q-left
                       q-right)]
           [else
            (merge-node
              (query-rec left-child-pos
                         left
                         mid
                         q-left
                         mid)
              (query-rec right-child-pos
                         (add1 mid)
                         right
                         (add1 mid)
                         q-right))]))])))

;; Entry :: (list value left right)
;; value => max - min

;; node-value :: Node -> Integer
(define (node-value node)
  (apply - node))

;; make-entry :: (Integer -> Integer -> Node) -> Integer -> Integer -> Entry
(define ((make-entry query-st) left right)
  (let ([node (query-st left right)])
    (list (node-value node) left right)))

;; entry>=? :: Entry -> Entry -> Boolean
(define (entry>=? et1 et2)
  (>= (car et1) (car et2)))

(define/contract (max-total-value nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (let* ([vec (list->vector nums)]
         [len (vector-length vec)]
         [st (make-st vec)]
         [query-st (query-st st len)]
         [make-entry (make-entry query-st)]
         [hp (make-heap entry>=?)])
    (for ([left (in-range len)])
      (heap-add! hp (make-entry left (sub1 len))))
    (let loop ([count k]
               [ans 0])
      (if (zero? count)
          ans
          (match-let ([(list value left right) (heap-min hp)])
            (heap-remove-min! hp)
            (when (> right left)
              (heap-add! hp (make-entry left (sub1 right))))
            (loop (sub1 count) (+ ans value)))))))
