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

(require data/heap)

;; list-node->list :: ListNode Integer -> [Integer]
(define/contract (list-node->list head)
  (-> (or/c list-node? #f) (listof exact-integer?))
  (if (not head)
      '()
      (cons (list-node-val head) (list-node->list (list-node-next head)))))

;; list->list-node :: [Integer] -> ListNode Integer
(define/contract (list->list-node xs)
  (-> (listof exact-integer?) (or/c list-node? #f))
  (if (null? xs)
      #f
      (list-node (car xs) (list->list-node (cdr xs)))))

;; list<= :: [Integer] -> [Integer] -> Boolean
(define/contract (list<= xs ys)
  (-> (listof exact-integer?) (listof exact-integer?) boolean?)
  (<= (car xs) (car ys)))

;; *merge-k-lists :: Heap [Integer] -> [Integer]
(define/contract (*merge-k-lists heap)
  (-> heap? (listof exact-integer?))
  (if (zero? (heap-count heap))
      '()
      (let ([xs (heap-min heap)])
        (heap-remove-min! heap)
        (if (null? (cdr xs))
            (cons (car xs) (*merge-k-lists heap))
            (begin
              (heap-add! heap (cdr xs))
              (cons (car xs) (*merge-k-lists heap)))))))

(define/contract (merge-k-lists lists)
  (-> (listof (or/c list-node? #f)) (or/c list-node? #f))
  (let ([heap (make-heap list<=)])
    (heap-add-all! heap (filter (compose not null?) (map list-node->list lists)))
    (list->list-node (*merge-k-lists heap))))
