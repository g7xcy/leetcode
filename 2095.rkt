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

;; list-node-length :: list-node a -> Integer
(define (list-node-length node)
  (if (not node)
      0
      (add1 (list-node-length (list-node-next node)))))

;; delete-at :: list-node a -> Integer -> list-node a
(define (delete-at node pos)
  (let delete-at-rec ([node node]
                      [cur 0])
    (cond
      [(not node) #f]
      [(= cur pos) (list-node-next node)]
      [else
       (list-node
         (list-node-val node)
         (delete-at-rec (list-node-next node) (add1 cur)))])))

(define/contract (delete-middle head)
  (-> (or/c list-node? #f) (or/c list-node? #f))
  (let* ([len (list-node-length head)]
         [mid (quotient len 2)])
    (delete-at head mid)))
