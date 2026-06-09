(require data/skip-list)

;; make-obstacles :: -> SkipList
(define (make-obstacles)
  (let ([sl (make-skip-list real-order)])
    (skip-list-set! sl 0 #t)
    sl))

;; prev-obstacle :: SkipList Integer -> Integer
(define (prev-obstacle obstacles x)
  (let ([it (skip-list-iterate-greatest/<=? obstacles x)])
    (skip-list-iterate-key obstacles it)))

;; next-obstacle :: SkipList Integer -> (Integer | #f)
(define (next-obstacle obstacles x)
  (let ([it (skip-list-iterate-least/>? obstacles x)])
    (if it
        (skip-list-iterate-key obstacles it)
        #f)))

;; add-obstacle! :: SkipList Integer -> Integer -> Void
(define (add-obstacle! obstacles x)
  (skip-list-set! obstacles x #t))


;; Segment Tree

;; make-tree :: Integer -> Vector Integer
(define (make-tree n)
  (make-vector (* n 4) 0))

;; pull! :: Vector Integer -> Integer -> Void
(define (pull! tree node)
  (vector-set! tree node
               (max (vector-ref tree (* 2 node))
                    (vector-ref tree (add1 (* 2 node))))))

;; update! :: Vector Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Void
(define (update! tree node left right index value)
  (if (= left right)
      (vector-set! tree node value)
      (let ([mid (quotient (+ left right) 2)])
        (if (<= index mid)
            (update! tree (* 2 node) left mid index value)
            (update! tree (add1 (* 2 node)) (add1 mid) right index value))
        (pull! tree node))))

;; query :: Vector Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
(define (query tree node left right query-left query-right)
  (cond
    [(or (< right query-left) (> left query-right)) 0]
    [(and (<= query-left left) (<= right query-right)) (vector-ref tree node)]
    [else
     (let ([mid (quotient (+ left right) 2)])
       (max
         (query tree (* 2 node) left mid query-left query-right)
         (query tree (add1 (* 2 node)) (add1 mid) right query-left query-right)))]))

;; max-coordinate :: List Query -> Integer
(define (max-coordinate queries)
  (foldl
    (lambda (q acc) (max acc (cadr q)))
    0
    queries))


;; process-queries ::
;; Vector Integer -> Integer -> List Query -> SkipList -> List Boolean -> List Boolean
(define (process-queries tree n queries obstacles results)
  (if (null? queries)
      (reverse results)
      (match-let ([(list new-results)
                   (process-query tree n (car queries) obstacles results)])
        (process-queries tree
                         n
                         (cdr queries)
                         obstacles
                         new-results))))

;; process-query ::
;; Vector Integer -> Integer -> Query -> SkipList -> List Boolean
;; -> (List List-Boolean)
(define (process-query tree n query obstacles results)
  (match query
    [(list 1 x) (process-add-obstacle tree n x obstacles results)]
    [(list 2 x sz) (process-check-block tree n x sz obstacles results)]))

;; process-add-obstacle ::
;; Vector Integer -> Integer -> Integer -> SkipList -> List Boolean
;; -> (List List-Boolean)
(define (process-add-obstacle tree n x obstacles results)
  (let ([prev (prev-obstacle obstacles x)]
        [next (next-obstacle obstacles x)])
    (update! tree 1 0 (sub1 n) x (- x prev))
    (when next
      (update! tree 1 0 (sub1 n) next (- next x)))
    (add-obstacle! obstacles x)
    (list results)))

;; process-check-block ::
;; Vector Integer -> Integer -> Integer -> Integer -> SkipList -> List Boolean
;; -> (List List-Boolean)
(define (process-check-block tree n x sz obstacles results)
  (let* ([prev (prev-obstacle obstacles x)]
         [best-complete-gap (query tree 1 0 (sub1 n) 0 prev)]
         [tail-gap (- x prev)]
         [best (max best-complete-gap tail-gap)]
         [can-place? (>= best sz)])
    (list (cons can-place? results))))

(define/contract (get-results queries)
  (-> (listof (listof exact-integer?)) (listof boolean?))
  (let* ([max-x (max-coordinate queries)]
         [n (add1 max-x)]
         [tree (make-tree n)]
         [obstacles (make-obstacles)])
    (process-queries tree n queries obstacles '())))
