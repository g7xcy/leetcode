(define (list->hash seq mat)
  (begin
    (for ([(v i) seq])
      (hash-set! mat i v)))
  mat)

(define (safe-string-ref s r)
  (if (<= (string-length s) r)
      '()
      (string-ref s r)))

(define/contract (valid-word-square words)
  (-> (listof string?) boolean?)
  (let
    ([max-length (max (length words) (foldl (lambda (s acc) (max acc (string-length s))) 0 words))]
     [mat (list->hash (in-indexed words) (make-hash))])
    (andmap
      (lambda (key)
        (let ([row (car key)]
              [column (cdr key)])
          (eq?
            (safe-string-ref (hash-ref mat row "") column)
            (safe-string-ref (hash-ref mat column "") row))))
      (for*/list
        ([row (in-range max-length)]
         [column (in-range max-length)])
        (cons row column)))))
