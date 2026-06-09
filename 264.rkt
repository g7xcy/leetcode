(require data/heap racket/set)

(define/contract (nth-ugly-number n)
  (-> exact-integer? exact-integer?)
  (let ([ugly-heap (make-heap <)]
        [ugly-set (mutable-set)])

    (heap-add! ugly-heap 1)
    (set-add! ugly-set 1)

    (let loop ([i 0] [cur 1])
      (if (= i n)
          cur
          (let ([x (heap-min ugly-heap)])
            (heap-remove-min! ugly-heap)

            (for ([k '(2 3 5)])
              (let ([y (* x k)])
                (unless (set-member? ugly-set y)
                  (set-add! ugly-set y)
                  (heap-add! ugly-heap y))))

            (loop (add1 i) x))))))