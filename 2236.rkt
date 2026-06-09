#lang racket

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

(define/contract (check-tree root)
  (-> (or/c tree-node? #f) boolean?)
  (if
    (tree-node? root)
    (=
      (tree-node-val root)
      (let (
            [lr (tree-node-left root)]
            [rr (tree-node-right root)])
        (+
          (if
            (tree-node? lr)
            (tree-node-val lr)
            0)
          (if
            (tree-node? rr)
            (tree-node-val rr)
            0))))
    #f))