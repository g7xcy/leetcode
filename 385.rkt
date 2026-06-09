#|
CFG:
(X) S        -> Integer | List
(X) Integer  -> Digit | - Digit | Integer Digit
(X) Digit    -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
(X) List     -> [ Elements ]
(X) Elements -> Element Rest | ε
(X) Rest     -> , Element Rest | ε
(X) Element  -> Integer | List
|#

;; Parser a :: String -> (a String) | #f
;; pure :: a -> Parser a
(define (pure a) (lambda (s) (cons a s)))
;; bind :: Parser a -> (a -> Parser b) -> Parser b
(define (bind p f)
  (lambda (s)
    (let ([res (p s)])
      (if (not res)
          #f
          ((f (car res)) (cdr res))))))
;; fmap :: (a -> b) -> Parser a -> Parser b
(define (fmap f p)
  (bind p
        (lambda (x) (pure (f x)))))
;; ignore-left :: Parser a -> Parser b -> Parser b
(define (ignore-left p q)
  (bind p
        (lambda (_) q)))
;; ignore-right :: Parser a -> Parser b -> Parser a
(define (ignore-right p q)
  (bind p
        (lambda (x)
          (bind q
                (lambda (_) (pure x))))))
;; choice :: Parser a -> Parser a -> Parser a
(define (choice p q)
  (lambda (s)
    (let ([res (p s)])
      (if (not res)
          (q s)
          res))))
;; some :: Parser a -> Parser [a]
;; some: 1+
(define (some p)
  (bind p
        (lambda (res-p)
          (bind (many p)
                (lambda (res-many-p)
                  (pure (cons res-p res-many-p)))))))
;; many :: Parser a -> Parser [a]
;; many: 0+
;; not accept (many (pure a))
(define (many p)
  (choice (some p) (pure '())))
;; satisfy :: (Char -> Boolean) -> Parser Char
;; test if first char of string satisfies the pred or-else fail
(define (satisfy pred)
  (lambda (s)
    (if (and
          (not (zero? (string-length s)))
          (pred (string-ref s 0)))
        (cons (string-ref s 0) (substring s 1))
        #f)))
;; parse-char :: Char -> Parser Char
(define (parse-char c) (satisfy (curry char=? c)))
;; char->digit :: Char -> Digit
(define (char->digit c)
  (- (char->integer c)
     (char->integer #\0)))
;; parse-digit :: Parser Digit
(define parse-digit (fmap char->digit (satisfy char-numeric?)))
(define parse-negative (fmap (const -1) (satisfy (curry char=? #\-))))
(define parse-sgn (choice parse-negative (pure 1)))
;; digits->integer :: [Digit] -> Integer
(define (digits->integer ds) (foldl (lambda (x acc) (+ (* 10 acc) x)) 0 ds))
;; parse-integer :: Parser Integer
(define parse-integer
  (bind parse-sgn
        (lambda (sgn)
          (fmap (curry * sgn)
                (fmap digits->integer (some parse-digit))))))
;; sepBy :: Parser a -> Parser sep -> Parser [a]
(define (sepBy p sep)
  (choice
    (bind p
          (lambda (x)
            (bind (many (ignore-left sep p))
                  (lambda (xs)
                    (pure (cons x xs))))))
    (pure '())))
(define parse-s
  (letrec
    ([parse-element
      (lambda (s)
        ((choice parse-integer parse-list) s))]
     [parse-elements
      (sepBy parse-element (parse-char #\,))]
     [parse-list
      (ignore-left (parse-char #\[)
                   (ignore-right parse-elements (parse-char #\])))])
    parse-element))

;; eval :: nested-integer% ->ast -> nested-integer%
(define (eval ast)
  (match ast
    ['() (new nested-integer%)]
    [`,digit
     (let ([ni (new nested-integer%)])
       (send ni set-integer digit)
       ni)]
    [`(,e ,rest)
     (let ([ni (new nested-integer%)])
       (send ni add (eval e))
       (let loop ([r rest])
         (match r
           ['() (send ni add (new nested-integer%))]
           [`(,e2 ,r2)
            (send ni add (eval e2))
            (loop r2)]))
       ni)]))
(define/contract (deserialize s)
  (-> string? (is-a?/c nested-integer%))
  (eval (car (parse-s s))))
