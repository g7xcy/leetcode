#|
CFG:
--------------------------------------------------------
(X) S       -> Base | Base Exp
(X) Base    -> Decimal | Integer
(X) Integer -> Sign Number | Number
(X) Sign    -> + | -
(x) Number  -> Digit | Digit Number
(x) Digit   -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
(X) Decimal -> Sign Rule | Rule
(X) Rule    -> Number . | Number . Number | . Number
(X) Exp     -> e Integer | E  Integer
--------------------------------------------------------
Parser a ≡ string -> (cons a rest) | #f
|#

; Parser :: a -> s -> ((cons a s) | #f)
; pure :: a -> Parser a
(define (pure _) identity)

; bind :: Parser a -> (a -> Parser b) -> Parser b
(define (bind p f)
  (lambda (s)
    (let ([res (p s)])
      (if (not res)
          #f
          ((f #t) res)))))

; choice : [Parser a] -> Parser a
(define (choice . ps)
  (lambda (s)
    (letrec
      ([rec
        (lambda (ps s)
          (if (null? ps)
              #f
              (let ([res ((car ps) s)])
                (if (not res)
                    (rec (cdr ps) s)
                    res))))])
      (rec ps s))))

; some 1+
; some :: Parser a → Parser (List a)
; some p = p >>= (\x -> many p >>= \xs -> pure [x xs])
(define (some p)
  (bind p
        (lambda (_)
          (many p))))

; many 0+
; many :: Parser a → Parser (List a)
; many p = some p <|> pure []
(define (many p) (choice (some p) (pure #t)))

(define (satisfy pred)
  (lambda (s)
    (if (and (> (string-length s) 0)
             (pred (string-ref s 0)))
        (substring s 1)
        #f)))
(define (parse-char c) (satisfy (lambda (x) (char=? x c))))
(define parse-digit (satisfy char-numeric?))
(define parse-number (some parse-digit))
(define parse-sign (choice (parse-char #\+) (parse-char #\-)))
(define parse-sign? (choice parse-sign (pure #t)))
(define parse-integer
  (bind parse-sign?
        (lambda (_) parse-number)))
(define parse-exp
  (bind (choice (parse-char #\e) (parse-char #\E))
        (lambda (_) parse-integer)))
(define parse-exp? (choice parse-exp (pure #t)))
; Number .
(define rule1
  (bind parse-number
        (lambda (_) (parse-char #\.))))
; Number . Number
(define rule2
  (bind parse-number
        (lambda (_)
          (bind (parse-char #\.)
                (lambda (_) parse-number)))))
; . Number
(define rule3
  (bind (parse-char #\.)
        (lambda (_) parse-number)))
(define parse-rule (choice rule2 rule1 rule3))
(define parse-decimal
  (bind parse-sign?
        (lambda (_) parse-rule)))
(define parse-base
  (choice parse-decimal parse-integer))
(define parse-s
  (bind parse-base
        (lambda (_) parse-exp?)))

(define/contract (is-number s)
  (-> string? boolean?)
  (let ([res (parse-s s)])
    (if (not res)
        #f
        (string=? res ""))))
