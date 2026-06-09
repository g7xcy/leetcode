#|
CFG:
--------------------------------------------------------
(X) Expr          -> Integer | LetExpr | AddExpr | MultExpr | Val

(X) Sign          -> + | -
(X) Digit         -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
(X) LowerCaseWord -> a | ... | z

(x) Number        -> Digit Number | Digit
(X) Integer       -> Sign Number | Number
(X) Val           -> LowerCaseWord EndVal
(X) EndVal        -> LowerCaseWord EndVal | Digit EndVal | ε

(X) AddExpr       -> ( add Expr Expr )
(X) MultExpr      -> ( mult Expr Expr )

(X) LetExpr       -> ( let LetBody )
(X) LetBody       -> Bindings Expr
(X) Bindings      -> Val Expr Bindings | ε
--------------------------------------------------------
|#

;; Parser :: a -> s -> ((cons a res) | #f)
;; pure :: a -> Parser a
(define (pure a)
  (lambda (s) (cons a s)))

;; bind :: Parser a -> (a -> Parser b) -> Parser b
(define (bind p f)
  (lambda (s)
    (let ([res (p s)])
      (if (not res)
          #f
          ((f (car res)) (cdr res))))))
(define (ignore-left p q)
  (bind p (lambda (_) q)))

;; choice : [Parser a] -> Parser [a]
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

;; some 1+
;; some :: Parser a → Parser (List a)
;; some p = p >>= (\x -> many p >>= \xs -> pure [x xs])
(define (some p)
  (bind p
        (lambda (x)
          (bind (many p)
                (lambda (xs)
                  (pure (cons x xs)))))))

;; many 0+
;; many :: Parser a → Parser (List a)
;; many p = some p <|> pure []
(define (many p)
  (choice
    (some p)
    (pure '())))

(define (fmap f p)
  (bind p (lambda (x) (pure (f x)))))

(define (satisfy pred)
  (lambda (s)
    (if (and (> (string-length s) 0)
             (pred (string-ref s 0)))
        (cons
          (string-ref s 0)
          (substring s 1))
        #f)))

(define (parse-char c)
  (satisfy (lambda (x) (char=? x c))))

(define (lazy p)
  (lambda (s)
    ((let ([f (if (procedure? p)
                  p
                  (lambda () (force p)))])
       (f))
     s)))

(define parse-expr
  (lazy
    (lambda ()
      (choice
        parse-integer
        parse-add
        parse-mult
        parse-let
        parse-val))))
(define parse-space (parse-char #\space))
(define parse-spaces (many parse-space))
(define parse-lp (parse-char #\())
(define parse-rp (parse-char #\)))
(define parse-lps (ignore-left parse-lp parse-spaces))
(define parse-srp (ignore-left parse-spaces parse-rp))
(define parse-digit
  (bind (satisfy char-numeric?)
        (lambda (d) (pure (- (char->integer d) (char->integer #\0))))))
(define parse-digits
  (some parse-digit))
(define parse-sign
  (bind (choice
          (parse-char #\+)
          (parse-char #\-))
        (lambda (sgn)
          (if (char=? sgn #\+)
              (pure 1)
              (pure -1)))))
(define parse-sign?
  (choice parse-sign (pure 1)))
(define parse-number
  (fmap
    (lambda (ds)
      (foldl
        (lambda (d acc) (+ (* acc 10) d))
        0
        ds))
    parse-digits))
(define parse-integer
  (bind parse-sign?
        (lambda (sgn)
          (bind parse-number
                (lambda (num)
                  (pure (* sgn num)))))))
(define parse-lowercase
  (satisfy
    (lambda (c)
      (and
        (>= (char->integer c) (char->integer #\a))
        (<= (char->integer c) (char->integer #\z))))))
(define parse-digit-char
  (satisfy char-numeric?))
(define parse-endval
  (many (choice parse-lowercase parse-digit-char)))
(define parse-val
  (bind parse-lowercase
        (lambda (c)
          (bind parse-endval
                (lambda (cs)
                  (pure (list->string (cons c cs))))))))
(define parse-bindings
  (many
    (bind parse-val
          (lambda (v)
            (ignore-left parse-spaces
                         (bind parse-expr
                               (lambda (e)
                                 (ignore-left parse-spaces
                                              (pure (list v e))))))))))
(define parse-letbody
  (bind parse-bindings
        (lambda (bd)
          (ignore-left parse-spaces
                       (bind parse-expr
                             (lambda (body)
                               (pure (list bd body))))))))
(define (parse-string s)
  (if (zero? (string-length s))
      (pure s)
      (bind (parse-char (string-ref s 0))
            (lambda (_)
              (ignore-left
                (parse-string (substring s 1))
                (pure s))))))
(define parse-letter-or-digit
  (choice parse-lowercase parse-digit-char))
(define parse-let-keyword
  (bind (parse-string "let")
        (lambda (_)
          (choice
            (bind parse-letter-or-digit
                  (lambda (_) (lambda (s) #f)))
            (pure 'let)))))
(define parse-let
  (ignore-left parse-lps
               (ignore-left parse-let-keyword
                            (ignore-left parse-spaces
                                         (bind parse-letbody
                                               (lambda (body)
                                                 (ignore-left parse-srp
                                                              (pure body))))))))
(define parse-add
  (ignore-left parse-lps
               (ignore-left (parse-string "add")
                            (ignore-left parse-spaces
                                         (bind parse-expr
                                               (lambda (e1)
                                                 (ignore-left parse-spaces
                                                              (bind parse-expr
                                                                    (lambda (e2)
                                                                      (ignore-left parse-srp
                                                                                   (pure (list 'add e1 e2))))))))))))

(define parse-mult
  (ignore-left parse-lps
               (ignore-left (parse-string "mult")
                            (ignore-left parse-spaces
                                         (bind parse-expr
                                               (lambda (e1)
                                                 (ignore-left parse-spaces
                                                              (bind parse-expr
                                                                    (lambda (e2)
                                                                      (ignore-left parse-srp
                                                                                   (pure (list 'mult e1 e2))))))))))))
;; eval : Expr × Env → Int
;; Env :: String -> Int
(define (empty-env)
  (lambda (x)
    (error "unbound variable" x)))

(define (extend-env env x v)
  (lambda (y)
    (if (string=? x y)
        v
        (env y))))

(define (eval-expr expr env)
  (cond
    ;; integer
    [(integer? expr) expr]
    ;; variable
    [(string? expr) (env expr)]
    ;; add
    [(and (list? expr) (eq? (car expr) 'add))
     (+ (eval-expr (cadr expr) env)
        (eval-expr (caddr expr) env))]
    ;; mult
    [(and (list? expr) (eq? (car expr) 'mult))
     (* (eval-expr (cadr expr) env)
        (eval-expr (caddr expr) env))]
    ;; let
    [(and (list? expr)
          (list? (car expr))) ; bindings
     (let* ([bindings (car expr)]
            [body (cadr expr)]
            [new-env
             (foldl
               (lambda (b acc-env)
                 (let ([x (car b)]
                       [e (cadr b)])
                   (extend-env acc-env x (eval-expr e acc-env))))
               env
               bindings)])
       (eval-expr body new-env))]
    [else
     (error "unknown expression" expr)]))

(define/contract (evaluate expression)
  (-> string? exact-integer?)
  (let ([ast (car (parse-expr expression))])
    (eval-expr ast (empty-env))))
