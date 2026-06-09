(define r1 '((0 0) (0 1) (0 2)))
(define r2 '((1 0) (1 1) (1 2)))
(define r3 '((2 0) (2 1) (2 2)))
(define c1 '((0 0) (1 0) (2 0)))
(define c2 '((0 1) (1 1) (2 1)))
(define c3 '((0 2) (1 2) (2 2)))
(define d1 '((0 0) (1 1) (2 2)))
(define d2 '((0 2) (1 1) (2 0)))

(define (eqs? s a b c)
  (and (eq? s a) (eq? a b) (eq? b c)))

(define (judge-one-state player m p)
  (let ([xs (filter (compose not zero?) (map (lambda (k) (hash-ref m k 0)) p))])
    (and (= 3 (length xs)) (apply eqs? player xs))))

(define (judge-state player m)
  (or
    (judge-one-state player m r1)
    (judge-one-state player m r2)
    (judge-one-state player m r3)
    (judge-one-state player m c1)
    (judge-one-state player m c2)
    (judge-one-state player m c3)
    (judge-one-state player m d1)
    (judge-one-state player m d2)))

(define (player-a-move m moves)
  (if (null? moves)
      m
      (player-b-move (hash-set m (car moves) 1) (cdr moves))))

(define (player-b-move m moves)
  (if (null? moves)
      m
      (player-a-move (hash-set m (car moves) -1) (cdr moves))))

(define/contract (tictactoe moves)
  (-> (listof (listof exact-integer?)) string?)
  (let
    ([m (player-a-move (hash) moves)])
    (cond
      [(judge-state 1 m) "A"]
      [(judge-state -1 m) "B"]
      [(= 9 (length moves)) "Draw"]
      [else "Pending"])))
