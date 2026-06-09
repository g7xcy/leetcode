; Definition for a binary tree node.
#|

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

|#

;; parents :: [[Integer, Integer, Integer]] -> Set Integer
(define (parents xss)
  (foldl
    (lambda (xs acc)
      (match xs
        [(list parent child is-left)
         (set-add acc parent)]))
    (set)
    xss))

;; children :: [[Integer, Integer, Integer]] -> Set Integer
(define (children xss)
  (foldl
    (lambda (xs acc)
      (match xs
        [(list parent child is-left)
         (set-add acc child)]))
    (set)
    xss))

;; get-node! :: Hash Integer tree-node -> Integer -> tree-node
(define (get-node! ht x)
  (hash-ref! ht x
             (lambda ()
               (make-tree-node x))))

;; build-tree-hash! :: Hash Integer tree-node -> [Integer, Integer, Integer] -> Void
(define (build-tree-hash! ht xs)
  (match xs
    [(list parent child is-left)
     (let ([parent-node (get-node! ht parent)]
           [child-node (get-node! ht child)])
       (if (= is-left 1)
           (set-tree-node-left! parent-node child-node)
           (set-tree-node-right! parent-node child-node)))]))

(define/contract (create-binary-tree descriptions)
  (-> (listof (listof exact-integer?)) (or/c tree-node? #f))
  (let ([root-val (set-first
                    (set-subtract
                      (parents descriptions)
                      (children descriptions)))]
        [tree-hash (make-hash)])
    (for-each
      (curry build-tree-hash! tree-hash)
      descriptions)
    (hash-ref tree-hash root-val)))
