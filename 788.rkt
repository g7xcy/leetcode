;; made by good nums and
;; not all 0, 1 or 8?
(define (is-validated-num x)
  (or (= x 0) (= x 1) (= x 2) (= x 5)
      (= x 6) (= x 8) (= x 9)))

(define (all-validated-num? xs)
  (null?
    (filter
      (lambda (x) (not (is-validated-num x)))
      xs)))

(define (all-bad-validated-num? xs)
  (null?
    (filter
      (lambda (x) (not (or (= x 0) (= x 1) (= x 8))))
      xs)))

(define (number->list num)
  (map
    (lambda (x) (- (char->integer x) (char->integer #\0)))
    (string->list (number->string num))))

(define (good-num? num)
  (let ([xs (number->list num)])
    (and
      (all-validated-num? xs)
      (not (all-bad-validated-num? xs)))))

(define/contract (rotated-digits n)
  (-> exact-integer? exact-integer?)
  (foldl
    (lambda (x acc)
      (if (good-num? x)
          (add1 acc)
          acc))
    0
    (range 2 (add1 n) 1)))
