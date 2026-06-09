;; empty-children :: Pair False
(define empty-children
  (cons #f #f))

;; set-left-child :: Pair Integer -> Pair Integer
(define (set-left-child old child)
  (cons child (cdr old)))

;; set-right-child :: Pair Integer -> Pair Integer
(define (set-right-child old child)
  (cons (car old) child))

;; update-children-map :: [Integer Integer Integer] -> Hash Integer Pair -> Hash Integer (Pair Integer)
(define (update-children-map xs children-map)
  (match xs
    [(list parent child is-left)
     (let ([old (hash-ref children-map parent empty-children)])
       (hash-set children-map
                 parent
                 (if (= is-left 1)
                     (set-left-child old child)
                     (set-right-child old child))))]))

;; parents :: [[Integer Integer Integer]] -> Set Integer
(define (parents xss)
  (foldl
    (lambda (xs acc) (set-add acc (car xs)))
    (set)
    xss))

;; children :: [[Integer Integer Integer]] -> Set Integer
(define (children xss)
  (foldl
    (lambda (xs acc) (set-add acc (cadr xs)))
    (set)
    xss))

;; build-tree :: Hash Integer Pair -> Integer -> tree-node
(define (build-tree children-map root-val)
  (let* ([children (hash-ref children-map root-val empty-children)]
         [left-val (car children)]
         [right-val (cdr children)])
    (tree-node root-val
               (and left-val
                    (build-tree children-map left-val))
               (and right-val
                    (build-tree children-map right-val)))))

(define/contract (create-binary-tree descriptions)
  (-> (listof (listof exact-integer?)) (or/c tree-node? #f))
  (let* ([root-val (set-first
                     (set-subtract
                       (parents descriptions)
                       (children descriptions)))]
         [children-map
          (foldl update-children-map
                 (hash)
                 descriptions)])
    (build-tree children-map root-val)))
