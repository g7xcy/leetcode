; Definition for singly-linked list:
#|

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

|#
(define (last-list-node? node)
  (or (and (list-node? node) (not (list-node-next node))))
  (not node))

(define/contract (merge-two-lists list1 list2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))
  (cond
    [(last-list-node? list1) list2]
    [(last-list-node? list2) list1]
    [else
     (let
       ([x (list-node-val list1)]
        [y (list-node-val list2)])
       (if (< x y)
           (list-node x (merge-two-lists (list-node-next list1) list2))
           (list-node y (merge-two-lists list1 (list-node-next list2)))))]))
