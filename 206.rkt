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

(define (list-node->list head)
  (cond
    [(not head) '()]
    [(not (list-node-next head)) (list (list-node-val head))]
    [else (cons (list-node-val head) (list-node->list (list-node-next head)))]))

(define (list->list-node xs)
  (if (null? xs)
      #f
      (list-node (car xs) (list->list-node (cdr xs)))))

(define/contract (reverse-list head)
  (-> (or/c list-node? #f) (or/c list-node? #f))
  (list->list-node (foldl cons '() (list-node->list head))))
