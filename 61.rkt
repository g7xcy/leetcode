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
  (if (not head)
      '()
      (cons (list-node-val head) (list-node->list (list-node-next head)))))

(define (list->list-node xs)
  (if (null? xs)
      #f
      (list-node (car xs) (list->list-node (cdr xs)))))

(define (shift1 xs)
  (append (cdr xs) (list (car xs))))

(define (shiftk k xs)
  (if (zero? k)
      xs
      (shiftk (sub1 k) (shift1 xs))))

(define/contract (rotate-right head k)
  (-> (or/c list-node? #f) exact-integer? (or/c list-node? #f))
  (let* ([xs (list-node->list head)]
         [l (length xs)])
    (if
      (or (null? xs) (zero? k))
      head
      (let* ([r (modulo k l)]
             [cut (- l r)])
        (if (zero? r)
            head
            (list->list-node
              (append (drop xs cut)
                      (take xs cut))))))))
