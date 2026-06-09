(define (transpose mat)
  (if (null? mat)
      '()
      (apply map list mat)))

(define/contract (spiral-order matrix)
  (-> (listof (listof exact-integer?)) (listof exact-integer?))
  (if (null? matrix)
      '()
      (append
        (car matrix)
        (spiral-order
          (reverse (transpose (cdr matrix)))))))

#|
1 2 3
4 5 6
7 8 9

1 2 3
6 9
5 8
4 7

1 2 3
6 9
8 7
5 4

1 2 3
6 9
8 7
4
5
|#