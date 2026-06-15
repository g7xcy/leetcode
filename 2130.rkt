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

;; list-node->list :: list-node->list -> List Integer
(define (list-node->list node)
  (if (not node)
      '()
      (cons (list-node-val node) (list-node->list (list-node-next node)))))

;; pair-sums :: List Integer -> List Integer
(define (pair-sums xs)
  (foldl
    (lambda (x y acc) (cons (+ x y) acc))
    '()
    xs
    (reverse xs)))

(define/contract (pair-sum head)
  (-> (or/c list-node? #f) exact-integer?)
  (let* ([xs (list-node->list head)]
         [pair-sums (pair-sums xs)])
    (apply max pair-sums)))
