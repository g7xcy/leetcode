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

(define/contract (invert-tree root)
  (-> (or/c tree-node? #f) (or/c tree-node? #f))
  (if (not root)
      #f
      (tree-node
        (tree-node-val root)
        (invert-tree (tree-node-right root))
        (invert-tree (tree-node-left root)))))
