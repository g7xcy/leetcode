(require data/queue)

;; ->vector :: [[Integer]] -> Vector Vector Integer
(define (->vector grid)
  (list->vector
    (map list->vector grid)))

;; at* :: Vector Vector Integer -> Integer -> Integer -> Integer
(define ((at* grid) row col)
  (vector-ref
    (vector-ref grid row)
    col))

;; update-at :: (Integer -> Integer -> Integer) -> Set [Integer] -> (Integer -> Integer -> Integer)
(define ((update-at at mandarini-marci) row col)
  (if (set-member? mandarini-marci (list row col))
      2
      (at row col)))

;; enqueue :: Queue Integer -> Integer -> Queue Integer
(define (enqueue queue val)
  (begin
    (enqueue! queue val)
    queue))

;; dequeue :: Queue Integer -> Values [Integer, Queue Integer]
(define (dequeue queue)
  (values (dequeue! queue) queue))

;; init-queue :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer -> Queue Integer -> Queue Integer
(define (init-queue at max-row max-col row col queue)
  (cond
    [(= max-row row) queue]
    [(= max-col col) (init-queue at max-row max-col (add1 row) 0 queue)]
    [else
     (if (= 2 (at row col))
         (init-queue at max-row max-col row (add1 col) (enqueue queue (list row col 0)))
         (init-queue at max-row max-col row (add1 col) queue))]))

;; in-grid :: Integer -> Integer -> Integer -> Integer -> [Integer] -> Boolean
(define ((in-grid* max-row max-col row col) direction)
  (let ([new-row (+ row (car direction))]
        [new-col (+ col (cadr direction))])
    (and
      (>= new-row 0)
      (>= new-col 0)
      (< new-row max-row)
      (< new-col max-col))))

;; mandarino-fresco? :: (Integer -> Integer -> Integer) -> [Integer] -> Boolean
(define ((mandarino-fresco?* at) direction)
  (= 1 (at (car direction) (cadr direction))))

;; vicini :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer -> Integer -> Queue Integer -> Values [(Integer -> Integer -> Integer), Queue Integer, Integer]
(define (vicini at max-row max-col row col depth queue)
  (let* ([directions '((0 -1) (-1 0) (0 1) (1 0))]
         [in-grid (in-grid* max-row max-col row col)]
         [mandarino-fresco? (mandarino-fresco?* at)]
         [next-mandarini (filter mandarino-fresco?
                                 (map
                                   (lambda (d) (list (+ row (car d)) (+ col (cadr d))))
                                   (filter in-grid directions)))])
    (values
      (update-at at (list->set next-mandarini))
      (foldl
        (lambda (mandarino acc)
          (enqueue acc (list (car mandarino) (cadr mandarino) (add1 depth))))
        queue
        next-mandarini)
      (length next-mandarini))))

;; bfs :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Queue Integer -> Integer -> Values [Integer, Integer]
(define (bfs at max-row max-col queue quantità-di-marciume)
  (let-values ([(p queue) (dequeue queue)])
    (let-values ([(at queue-nuova quantità-di-marciume-nuova) (vicini at max-row max-col (car p) (cadr p) (caddr p) queue)])
      (if (queue-empty? queue-nuova)
          (values (caddr p) quantità-di-marciume)
          (bfs at max-row max-col queue-nuova (+ quantità-di-marciume quantità-di-marciume-nuova))))))

(define/contract (oranges-rotting grid)
  (-> (listof (listof exact-integer?)) exact-integer?)
  (let* ([quantità-di-mandarini-freschi
          (length (filter (curry = 1) (flatten grid)))]
         [max-row (length grid)]
         [max-col (length (car grid))]
         [grid* (->vector grid)]
         [at (at* grid*)]
         [queue (init-queue at max-row max-col 0 0 (make-queue))])
    (cond
      [(zero? quantità-di-mandarini-freschi) 0]
      [(queue-empty? queue) -1]
      [else
       (let-values ([(minutes quantità-di-marciume)
                     (bfs at max-row max-col queue 0)])
         (if (< quantità-di-marciume quantità-di-mandarini-freschi)
             -1
             minutes))])))
