(define (string-contains? s sub)
  (let ([n (string-length s)]
        [m (string-length sub)])
    (let loop ([i 0])
      (cond
        [(> (+ i m) n) #f]
        [(string=? (substring s i (+ i m)) sub) #t]
        [else (loop (add1 i))]))))

(define/contract (rotate-string s goal)
  (-> string? string? boolean?)
  (and
    (= (string-length s) (string-length goal))
    (or
      (string=? s goal)
      (let*
        ([ss (string-append s s)]
         [len (string-length ss)])
        (string-contains?
          (substring ss 1 (sub1 len))
          goal)))))
