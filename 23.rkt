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

;; merge-2-lists :: [Integer] -> [Integer] -> [Integer]
(define/contract (merge-2-lists xs ys)
  (-> (listof exact-integer?) (listof exact-integer?) (listof exact-integer?))
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    [(< (car xs) (car ys)) (cons (car xs) (merge-2-lists (cdr xs) ys))]
    [else (cons (car ys) (merge-2-lists xs (cdr ys)))]))

;; merge-pairs :: [[Integer]] -> [[Integer]]
(define/contract (merge-pairs lists)
  (-> (listof (listof exact-integer?)) (listof (listof exact-integer?)))
  (cond
    [(null? lists) '()]
    [(null? (cdr lists)) lists]
    [else
     (cons
       (merge-2-lists (car lists) (cadr lists))
       (merge-pairs (cddr lists)))]))

;; *merge-k-lists :: [[Integer]] -> [Integer]
(define/contract (*merge-k-lists lists)
  (-> (listof (listof exact-integer?)) (listof exact-integer?))
  (cond
    [(null? lists) '()]
    [(null? (cdr lists)) (car lists)]
    [else
     (*merge-k-lists (merge-pairs lists))]))

(define/contract (merge-k-lists lists)
  (-> (listof (or/c list-node? #f)) (or/c list-node? #f))
  (list->list-node
    (*merge-k-lists
      (map list-node->list lists))))
