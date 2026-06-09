;; roll-onion :: [[Integer]] -> [[Integer]]
(define (roll-onion onion)
  (reverse (apply map list onion)))

;; empty-onion? :: [[Integer]] -> Boolean
(define (empty-onion? onion)
  (or (null? onion)
      (null? (car onion))))

;; normalize-onion :: [[Integer]] -> [[Integer]]
(define (normalize-onion onion)
  (if (or (null? onion)
          (null? (car onion)))
      '()
      onion))

;; drop-skin :: [[Integer]] -> [[Integer]]
(define (drop-skin onion)
  (if (empty-onion? onion)
      '()
      (normalize-onion
        (map (lambda (xs) (drop-right (cdr xs) 1)) (drop-right (cdr onion) 1)))))

;; peel-skin :: [[Integer]] -> Integer -> [Integer]
(define (peel-skin onion)
  (letrec
    ([peel-skin* (lambda (onion time)
                   (cond
                     [(empty-onion? onion) '()]
                     [(= 3 time) (drop-right (car onion) 1)]
                     [else (let ([roll-onioned-onion (roll-onion onion)])
                             (append (car onion) (cdr (peel-skin* roll-onioned-onion (add1 time)))))]))])
    (peel-skin* onion 0)))

;; peel-onion :: [[Integer]] -> [[Integer]]
(define (peel-onion onion)
  (if (empty-onion? onion)
      '()
      (cons (peel-skin onion) (peel-onion (drop-skin onion)))))

;; shift-skin :: Integer -> [Integer] -> [Integer]
(define (shift-skin k skin)
  (let ([shift (modulo k (length skin))])
    (append (drop skin shift) (take skin shift))))

;; shift-skin :: Integer -> [[Integer]] -> [[Integer]]
(define (shift-skins k skins)
  (map (curry shift-skin k) skins))

;; stick-sides :: [[Integer]] -> [Integer] -> [Integer] -> [[Integer]]
(define (stick-sides inner-onion left right)
  (cond
    [(null? left) '()]
    [else
     (let* ([inner-row (if (empty-onion? inner-onion)
                           '()
                           (car inner-onion))]
            [rest-inner (if (empty-onion? inner-onion)
                            '()
                            (cdr inner-onion))]
            [one-layer (append
                         (cons (car left) inner-row)
                         (list (car right)))])
       (cons one-layer
             (stick-sides rest-inner
                          (cdr left)
                          (cdr right))))]))

;; wrap-onion :: [Integer] -> Integer -> Integer -> [[Integer]] -> [[Integer]]
(define (wrap-onion skin row col inner-onion)
  (let* ([right-len (- row 2)]
         [bottom-start (+ col right-len)]

         [top (take skin col)]
         [right (take (drop skin col) right-len)]
         [bottom (reverse (take (drop skin bottom-start) col))]
         [left (reverse (take-right skin right-len))]

         [middle-onion (stick-sides inner-onion left right)])
    (append (cons top middle-onion) (list bottom))))

;; wrap-onions :: [[Integer]] -> Integer -> Integer -> [[Integer]]
(define (wrap-onions skins row col)
  (if (or (null? skins)
          (<= row 0)
          (<= col 0))
      '()
      (wrap-onion (car skins) row col
                  (wrap-onions (cdr skins)
                               (- row 2)
                               (- col 2)))))

(define/contract (rotate-grid grid k)
  (-> (listof (listof exact-integer?)) exact-integer? (listof (listof exact-integer?)))
  (let* ([row (length grid)]
         [col (length (car grid))]
         [skins (peel-onion grid)]
         [shifted-skins (shift-skins k skins)])
    (wrap-onions shifted-skins row col)))
