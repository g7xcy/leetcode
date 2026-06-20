;; restriction<? :: (Integer, Integer) -> (Integer, Integer) -> Boolean
(define (restriction<? restriction-1 restriction-2)
  (match-let ([(list index-1 _) restriction-1]
              [(list index-2 _) restriction-2])
    (< index-1 index-2)))

;; expand-restrictions :: List (Integer, Integer) -> List (Integer, Integer)
;; 1-pos
(define (expand-restrictions n sorted-restrictions)
  (match-let* ([1-pos-restrictions (cons '(1 0) sorted-restrictions)]
               [last-restriction (last 1-pos-restrictions)]
               [(list last-index _) last-restriction])
    (if (= n last-index)
        1-pos-restrictions
        (append 1-pos-restrictions (list (list n (sub1 n)))))))

;; propagate :: List (List Integer Integer) -> List (List Integer Integer)
(define (propagate restrictions)
  (match restrictions
    [(list prev curr rest ...)
     (match-let ([(list prev-index prev-max-height) prev]
                 [(list curr-index curr-max-height) curr])
       (let* ([distance (abs (- curr-index prev-index))]
              [fixed-height (min curr-max-height (+ prev-max-height distance))]
              [fixed-curr (list curr-index fixed-height)])
         (cons prev (propagate (cons fixed-curr rest)))))]
    [else restrictions]))

;; make-strict-restrictions :: List (Integer, Integer) -> List (Integer, Integer)
(define (make-strict-restrictions restrictions)
  (let* ([left-fixed (propagate restrictions)]
         [right-fixed (reverse (propagate (reverse left-fixed)))])
    right-fixed))

;; interval-max :: (Integer, Integer) -> (Integer, Integer) -> Integer
(define (interval-max left right)
  (match-let ([(list left-index left-height) left]
              [(list right-index right-height) right])
    (quotient (+ left-height right-height (- right-index left-index)) 2)))

;; max-interval-height :: List (Integer, Integer) -> Integer
(define (max-interval-height restrictions)
  (match restrictions
    [(list _) 0]
    [(list left right rest ...)
     (max (interval-max left right)
          (max-interval-height (cons right rest)))]))

(define/contract (max-building n restrictions)
  (-> exact-integer? (listof (listof exact-integer?)) exact-integer?)
  (let* ([sorted-restrictions (sort restrictions restriction<?)]
         [expanded-restrictions (expand-restrictions n sorted-restrictions)]
         [strict-restrictions (make-strict-restrictions expanded-restrictions)])
    (max-interval-height strict-restrictions)))
