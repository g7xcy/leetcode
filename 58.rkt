(define/contract (length-of-last-word s)
  (-> string? exact-integer?)
  (string-length (car (reverse (string-split (string-trim s) " ")))))