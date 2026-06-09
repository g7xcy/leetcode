;; trie-node :: (Integer | False, MutHash Char trie-node)
(struct trie-node (best children) #:mutable #:transparent)

;; make-trie-node :: trie-node
(define (make-trie-node)
  (trie-node #f (make-hash)))

;; string->reversed-list :: String -> List Char
(define (strings->reversed-list str)
  (reverse
    (string->list str)))

;; strings->length-vec :: (Listof String) -> (Vectorof Integer)
(define (strings->length-vec strings)
  (list->vector
    (map string-length strings)))

;; better-index:: Vector Integer -> (Integer | False) -> Integer -> Integer
(define (better-index lens old new)
  (cond
    [(not old) new]
    [(< (vector-ref lens new) (vector-ref lens old)) new]
    [(and (= (vector-ref lens new) (vector-ref lens old)) (< new old)) new]
    [else old]))

;; get-or-create-child! :: trie-node -> Char -> trie-node
(define (get-or-create-child! node ch)
  (hash-ref!
    (trie-node-children node)
    ch
    make-trie-node))

;; insert! :: trie-node -> Vector Integer -> List Char -> Integer -> Void
(define (insert! root lens chars index)
  (let loop ([cur root]
             [chars chars])
    (set-trie-node-best!
      cur
      (better-index lens (trie-node-best cur) index))
    (match chars
      ['() (void)]
      [(cons ch rest)
       (loop (get-or-create-child! cur ch) rest)])))

;; make-trie :: List (List Char) -> Vector Integer -> trie-node
(define (make-trie reversed-list lens)
  (let ([root (make-trie-node)])
    (for ([chars reversed-list]
          [index (in-naturals)])
      (insert! root lens chars index))
    root))

;; query-trie :: trie-node -> List Char -> Integer
(define ((query-trie root) chars)
  (let loop ([cur root]
             [chars chars]
             [ans (trie-node-best root)])
    (match chars
      ['() ans]
      [(cons ch rest)
       (define next
         (hash-ref (trie-node-children cur) ch #f))
       (if next
           (loop next rest (trie-node-best next))
           ans)])))

(define/contract (string-indices wordsContainer wordsQuery)
  (-> (listof string?) (listof string?) (listof exact-integer?))
  (let* ([reversed-container-word-list (map strings->reversed-list wordsContainer)]
         [container-word-length (strings->length-vec wordsContainer)]
         [root (make-trie reversed-container-word-list container-word-length)]
         [query-trie (query-trie root)])
    (map (compose query-trie reverse string->list)
         wordsQuery)))
