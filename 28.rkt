(define/contract (str-str haystack needle)
  (-> string? string? exact-integer?)
  (define n (string-length haystack))
  (define m (string-length needle))

  (cond
    [(zero? m) 0]
    [(< n m) -1]
    [else
     (define base 131)
     (define mod 1000000007)

     (define (char->num c)
       (char->integer c))

     (define base-expt
       (for/fold ([acc 1]) ([i (in-range (sub1 m))])
         (modulo (* acc base) mod)))

     (define (hash-window s start len)
       (for/fold ([h 0]) ([i (in-range start (+ start len))])
         (modulo (+ (* h base)
                    (char->num (string-ref s i)))
                 mod)))

     (define needle-hash
       (hash-window needle 0 m))

     (define init-hash
       (hash-window haystack 0 m))

     (define (roll-hash prev-hash out-char in-char)
       (modulo
         (+ (* (modulo (- prev-hash
                          (modulo (* (char->num out-char) base-expt) mod))
                       mod)
               base)
            (char->num in-char))
         mod))

     (define (window=? start)
       (string=?
         (substring haystack start (+ start m))
         needle))

     (let loop ([i 0] [h init-hash])
       (cond
         [(and (= h needle-hash) (window=? i)) i]
         [(= i (- n m)) -1]
         [else
          (loop (add1 i)
                (roll-hash h
                           (string-ref haystack i)
                           (string-ref haystack (+ i m))))]))]))
