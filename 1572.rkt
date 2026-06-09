(define (convert-to-hash-mat xs l r c)
  (if (null? xs)
      (make-hash)
      (if (< c l)
          (let ([mat (convert-to-hash-mat (cdr xs) l r (add1 c))])
            (hash-set! mat (cons r c) (car xs))
            mat)
          (let ([mat (convert-to-hash-mat (cdr xs) l (add1 r) 1)])
            (hash-set! mat (cons (add1 r) 0) (car xs))
            mat))))

(define (get-d1 mat l r c)
  (if (and (= l r) (= l c))
      0
      (+ (get-d1 mat l (add1 r) (add1 c)) (hash-ref mat (cons r c)))))

(define (get-d2 mat l r c)
  (if (and (= l r) (< c 0))
      0
      (+ (get-d2 mat l (add1 r) (sub1 c)) (hash-ref mat (cons r c)))))

(define/contract (diagonal-sum mat)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (let*
    ([l (length mat)]
     [mat-hash (convert-to-hash-mat (apply append mat) l 0 0)])
    (+
      (get-d1 mat-hash l 0 0)
      (get-d2 mat-hash l 0 (sub1 l))
      (if (even? l)
          0
          (- (hash-ref mat-hash (cons (/ (sub1 l) 2) (/ (sub1 l) 2))))))))
