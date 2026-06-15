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

;; (a -> b -> b) -> b -> ListNode a -> b
(define (foldl-list-node f init node)
  (if (not node)
      init
      (foldl-list-node f (f (list-node-val node) init) (list-node-next node))))

;; max-zip-apply :: ListNode (Integer -> Integer) -> ListNode Integer -> Integer -> Integer
(define (max-zip-apply adders node best)
  (if (or (not adders) (not node))
      best
      (max-zip-apply
        (list-node-next adders)
        (list-node-next node)
        (max best
             ((list-node-val adders)
              (list-node-val node))))))

;; make-adders :: ListNode -> ListNode (Integer -> Integer)
(define (make-adders node)
  (foldl-list-node
    (lambda (x acc) (list-node (curry + x) acc))
    #f
    node))

(define/contract (pair-sum head)
  (-> (or/c list-node? #f) exact-integer?)
  (let ([adders (make-adders head)])
    (max-zip-apply adders head 0)))
