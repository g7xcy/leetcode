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

(define (add-carry xs carry)
  (cond
    [(and (null? xs) (zero? carry)) '()]
    [(null? xs) (list carry)]
    [(zero? carry) xs]
    [else (let ([temp (+ (car xs) carry)])
            (if (>= temp 10)
                (cons (remainder temp 10) (add-carry (cdr xs) 1))
                (cons temp (cdr xs))))]))

(define (add-two-numbers-list xs ys carry)
  (cond
    [(null? xs) (add-carry ys carry)]
    [(null? ys) (add-carry xs carry)]
    [else
     (let ([temp (+ (car xs) (car ys) carry)])
       (cons (remainder temp 10) (add-two-numbers-list (cdr xs) (cdr ys) (quotient temp 10))))]))

(define/contract (add-two-numbers l1 l2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))
  (list->list-node
    (reverse
      (add-two-numbers-list
        (reverse (list-node->list l1))
        (reverse (list-node->list l2))
        0))))
