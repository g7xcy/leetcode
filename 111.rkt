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

(define/contract (min-depth root)
  (-> (or/c tree-node? #f) exact-integer?)
  (cond
    [(not root) 0]
    [(and (not (tree-node-left root)) (not (tree-node-right root))) 1]
    [(not (tree-node-left root)) (add1 (min-depth (tree-node-right root)))]
    [(not (tree-node-right root)) (add1 (min-depth (tree-node-left root)))]
    [else (add1 (min (min-depth (tree-node-right root)) (min-depth (tree-node-left root))))]))
