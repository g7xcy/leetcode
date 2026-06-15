;; weight-map :: List Integer -> Hash Char Integer
(define (weight-map weights)
  (let ([alphabet (map integer->char (inclusive-range (char->integer #\a) (char->integer #\z)))])
    (apply hash
           (foldr
             (lambda (letter weight acc) (cons letter (cons weight acc)))
             '()
             alphabet
             weights))))

;; calc-weight :: Hash Char Integer -> String -> Integer
(define ((calc-weight weight-map) word)
  (apply +
         (map (lambda (letter) (hash-ref weight-map letter))
              (string->list word))))

;; Hash Char Integer -> String -> Integer -> Char
(define ((word->letter calc-weight) word)
  (integer->char
    (- (char->integer #\z)
       (modulo (calc-weight word) 26))))

(define/contract (map-word-weights words weights)
  (-> (listof string?) (listof exact-integer?) string?)
  (let* ([weight-map (weight-map weights)]
         [calc-weight (calc-weight weight-map)]
         [word->letter (word->letter calc-weight)])
    (let rec ([words words])
      (if (null? words)
          ""
          (string-append (string (word->letter (car words)))
                         (rec (cdr words)))))))
