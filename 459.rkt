(define/contract (repeated-substring-pattern s)
  (-> string? boolean?)
  (string-contains (substring (string-append s s) 1 (sub1 (* 2 (string-length s)))) s))
