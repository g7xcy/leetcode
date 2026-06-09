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

;; list->list-node :: [Integer] -> ListNode Integer
(define (list->list-node xs)
  (if (null? xs)
      #f
      (list-node (car xs) (list->list-node (cdr xs)))))

;; bucket-sort :: ListNode Integer -> Hash Integer Integer -> Hash Integer Integer
(define (bucket-sort head bucket)
  (if head
      (begin
        (hash-update! bucket (list-node-val head) add1 0)
        (bucket-sort (list-node-next head) bucket))
      bucket))

(define/contract (merge-k-lists lists)
  (-> (listof (or/c list-node? #f)) (or/c list-node? #f))
  (let ([bucket (make-hash)])
    (for ([ls lists])
      (bucket-sort ls bucket))
    ((compose list->list-node flatten)
     (foldr
       (lambda (v acc)
         (let ([freq (hash-ref bucket v 0)])
           (if (zero? freq)
               acc
               (cons (make-list freq v) acc))))
       '()
       (inclusive-range -10000 10000)))))
