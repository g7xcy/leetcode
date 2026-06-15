(define ((char-weight weight-vec) ch)
  (vector-ref weight-vec
              (- (char->integer ch)
                 (char->integer #\a))))

(define ((word->letter char-weight) word)
  (define weight
    (for/sum ([ch (in-string word)])
      (char-weight ch)))
  (integer->char
    (- (char->integer #\z)
       (modulo weight 26))))

(define/contract (map-word-weights words weights)
  (-> (listof string?) (listof exact-integer?) string?)
  (let* ([weight-vec (list->vector weights)]
         [char-weight (char-weight weight-vec)]
         [word->letter (word->letter char-weight)])
    (list->string
      (map word->letter words))))