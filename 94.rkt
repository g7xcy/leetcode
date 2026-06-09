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

(define (inorder-traversal root)
  (if (not root)
      '()
      (append
        (inorder-traversal (tree-node-left root))
        (cons (tree-node-val root)
              (inorder-traversal (tree-node-right root))))))
