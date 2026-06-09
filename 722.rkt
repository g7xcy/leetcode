#|
(X) Comment          -> LineComment | BlockComment
(X) LineCStart       -> / /
(X) LineCEnd         -> \n
(X) LineComment      -> LineCStart LineCBody
(X) LineCBody        -> Char LineCBody | LineCEnd
(X) BlockCStart      -> / *
(X) BlockCEnd        -> * /
(X) BlockComment     -> BlockCStart BlockCBody
(X) BlockCBody       -> Char BlockCBody | BlockCEnd
|#
;; Parser a = String -> (a String) | #f
;; pure :: a -> Parser a
(define (pure a) (lambda (s) (cons a s)))
;; bind :: Parser a -> (a -> Parser b) -> Parser b
(define (bind p f)
  (lambda (s)
    (let ([res (p s)])
      (if (not res)
          #f
          ((f (car res)) (cdr res))))))
;; fmap :: Parser a -> (a -> b) -> Parser b
(define (fmap p f)
  (bind p
        (lambda (x) (pure (f x)))))
;; ignore-left :: Parser a -> Parser b -> Parser a
(define (ignore-left p q) (bind p (const q)))
(define >> ignore-left)
;; ignore :: Parser a -> Parser a
(define (ignore p) (fmap p (const '())))
;; satisfy :: (Char -> Boolean) -> Parser Char
(define (satisfy pred)
  (lambda (s)
    (if
      (or
        (zero? (string-length s))
        (not (pred (string-ref s 0))))
      #f
      (cons (string-ref s 0) (substring s 1)))))
;; choice :: Parser a -> Parser a -> Parser a
(define (choice p q)
  (lambda (s)
    (let ([res (p s)])
      (if (not res)
          (q s)
          res))))
(define ? choice)
;; some :: Parser a -> Parser [a]
;; some: 1+
(define (some p)
  (bind p
        (lambda (res)
          (bind (many p)
                (lambda (rest)
                  (pure (cons res rest)))))))
;; many :: Parser a -> Parser [a]
;; many: 0+
(define (many p) (? (some p) (pure '())))
(define 0+ many)
;; parse-char :: Char -> Parser Char
(define (parse-char c) (satisfy (curry char=? c)))
;; parse-any-char :: Char -> Parser Char
(define parse-any-char (satisfy (const #t)))
;; parse-string2 :: Char -> Char -> Parser String
(define (parse-string2 a b)
  (bind (parse-char a)
        (const
          (bind (parse-char b)
                (const
                  (pure (cons a b)))))))
;; not-followed-by2 :: Char -> Char -> Parser String
;; if the next two input are a and b,
;; then fail
;; or-else parse true but not consume
;; actually it is a if condition on Parser Monad
(define (not-followed-by2 a b)
  (lambda (s)
    (if (and (>= (string-length s) 2)
             (char=? (string-ref s 0) a)
             (char=? (string-ref s 1) b))
        #f
        (cons #t s))))

(define parse-linec-start (parse-string2 #\/ #\/))
(define parse-linec-end (parse-char #\newline))
(define parse-linec-body
  (0+ (satisfy (lambda (c) (not (char=? c #\newline))))))
(define parse-line-comment
  (fmap
    (>> parse-linec-start
        (>> parse-linec-body
            parse-linec-end))
    (const (list #\newline))))
(define parse-blockc-start (parse-string2 #\/ #\*))
(define parse-blockc-end (parse-string2 #\* #\/))
(define parse-blockc-body
  (0+
    (bind (not-followed-by2 #\* #\/)
          (const parse-any-char))))
(define parse-block-comment
  (ignore
    (>> parse-blockc-start
        (>> parse-blockc-body parse-blockc-end))))
(define parse-comment (? parse-line-comment parse-block-comment))
(define parse-code
  (bind (not-followed-by2 #\/ #\/)
        (const
          (bind (not-followed-by2 #\/ #\*)
                (const
                  (fmap parse-any-char list))))))
(define parse-segment
  (?
    parse-comment
    parse-code))
(define parse-program (0+ parse-segment))

(define (result source)
  (map car
       (filter
         (compose not null?)
         (car
           (parse-program
             (string-join source (string #\newline)))))))

(define (split-lines lines)
  (filter (compose not null?)
          (foldr
            (lambda (x acc)
              (match acc
                [(cons current rest)
                 (if (char=? x #\newline)
                     (cons '() (cons current rest))
                     (cons (cons x current) rest))]))
            (list '())
            lines)))
(define (output lines) (map list->string lines))

(define/contract (remove-comments source)
  (-> (listof string?) (listof string?))
  (output (split-lines (result source))))
