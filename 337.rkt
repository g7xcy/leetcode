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

(define/contract (rob root)
  (-> (or/c tree-node? #f) exact-integer?)
  (letrec
    ([loop (lambda (r)
             (if (not (tree-node? r))
                 (list 0 0)
                 (let* (
                        [left-res (loop (tree-node-left r))]
                        [right-res (loop (tree-node-right r))]
                        [left-skip (car left-res)]
                        [left-keep (cadr left-res)]
                        [right-skip (car right-res)]
                        [right-keep (cadr right-res)])
                   (list
                     (+ (max left-skip left-keep) (max right-skip right-keep))
                     (+ (tree-node-val r) left-skip right-skip)))))])
    (apply max (loop root))))
