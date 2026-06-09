#|
(X) S             -> ConditionExpr ? S : S | Digit | ConditionExpr
(X) Digit         -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
(X) ConditionExpr -> T | F
|#
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
;; choice :: Parser a -> Parser a -> Parser a
(define (choice p1 p2)
  (lambda (s)
    (let ([res (p1 s)])
      (if (not res)
          (p2 s)
          res))))
;; ignore-left :: Parser a -> Parser a -> Parser a
(define (ignore-left p q) (bind p (lambda (_) q)))
;; ignore-right :: Parser a -> Parser a -> Parser a
(define (ignore-right p q)
  (bind p
        (lambda (x)
          (bind q
                (lambda (_) (pure x))))))
;; some :: Parser a -> Parser [a]
;; some: 1+
(define (some p)
  (bind p
        (lambda (x)
          (bind (many p)
                (lambda (xs) (pure (cons x xs)))))))
;; many :: Parser a -> Parser [a]
;; many: 0+
(define (many p) (choice (some p) (pure '())))
;; satisfy :: (Char -> Boolean) -> Parser Char
(define (satisfy pred)
  (lambda (s)
    (if (and (> (string-length s) 0)
             (pred (string-ref s 0)))
        (cons (string-ref s 0) (substring s 1))
        #f)))
;; parse-char :: Char -> Parser Char
(define (parse-char c) (satisfy (lambda (x) (char=? c x))))
;; parse-t :: Parser Char
(define parse-t (fmap (lambda (_) '(bool t)) (parse-char #\T)))
;; parse-f :: Parser Char
(define parse-f (fmap (lambda (_) '(bool f)) (parse-char #\F)))
;; parse-condition :: Parser Char
(define parse-condition (choice parse-t parse-f))
;; parse-digit :: Parser Digit
(define parse-digit
  (bind (satisfy char-numeric?)
        (lambda (d)
          (pure `(digit ,(char->integer d))))))
(define parse-condition-or-digit (choice parse-condition parse-digit))
;; parse-s :: Parser String
(define parse-s
  (choice
    (bind parse-condition
          (lambda (condition)
            (ignore-left (parse-char #\?)
                         (bind parse-s
                               (lambda (then-branch)
                                 (ignore-left (parse-char #\:)
                                              (bind parse-s
                                                    (lambda (else-branch)
                                                      (pure `(if ,condition ,then-branch ,else-branch))))))))))
    parse-condition-or-digit))
(define (eval ast)
  (match ast
    [`(digit ,n) (string (integer->char n))]
    ['(bool t) #t]
    ['(bool f) #f]
    [`(if ,c ,t ,e)
     (if (eval c)
         (eval t)
         (eval e))]))
(define/contract (parse-ternary expression)
  (-> string? string?)
  (let ([ans (eval (car (parse-s expression)))])
    (if (string? ans)
        ans
        (if ans
            "T"
            "F"))))
