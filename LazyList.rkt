#lang racket

(struct lnil () #:transparent)
(struct lcons (head tail-promise) #:transparent)

;; lfirst :: LazyList a -> a
(define (lfirst xs)
  (lcons-head xs))

;; lrest :: LazyList a -> LazyList a
(define (lrest xs)
  (force (lcons-tail-promise xs)))

;; lmap :: (a -> b) -> LazyList a -> LazyList b
(define (lmap f xs)
  (if (lnil? xs)
      (lnil)
      (lcons
        (f (lfirst xs))
        (lazy (lmap f (lrest xs))))))

;; lfilter :: (a -> Boolean) -> LazyList a -> LazyList a
(define (lfilter pred xs)
  (cond
    [(lnil? xs) (lnil)]
    [(pred (lfirst xs))
     (lcons
       (lfirst xs)
       (lazy (lfilter pred (lrest xs))))]
    [else (lfilter pred (lrest xs))]))

;; ltake :: LazyList a -> Integer -> LazyList a
(define (ltake xs n)
  (cond
    [(zero? n) (lnil)]
    [(lnil? xs) (lnil)]
    [(lcons? xs)
     (lcons (lcons-head xs)
            (lazy
              (ltake (force (lcons-tail-promise xs))
                     (sub1 n))))]))

;; lazy-list->list :: LazyList a -> List a
(define (lazy-list->list xs)
  (if (lnil? xs)
      '()
      (cons (lfirst xs) (lazy-list->list (lrest xs)))))

;; list->lazy-list :: List a -> LazyList a
(define (list->lazy-list xs)
  (if (null? xs)
      (lnil)
      (lcons
        (car xs)
        (lazy (list->lazy-list (cdr xs))))))

;; iterate :: (a -> a) -> a -> LazyList a
(define (iterate f x)
  (lcons x
         (lazy (iterate f (f x)))))

;; from :: Integer -> LazyList Integer
(define (from n)
  (iterate add1 n))

(define naturals (from 0))
