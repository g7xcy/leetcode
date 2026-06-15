;; foldl* :: (a -> b -> b) -> b -> List a -> b
(define (foldl* f init xs)
  ((foldr (lambda (x acc) (compose acc (curry f x))) identity xs) init))
  
;; foldr* :: (a -> b -> b) -> b -> List a -> b
(define (foldr* f init xs)
  ((foldl (lambda (x acc) (compose acc (curry f x))) identity xs) init))