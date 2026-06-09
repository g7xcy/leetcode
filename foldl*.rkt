;; foldl* :: (a -> b -> b) -> b -> List a -> b
(define (foldl* f init xs)
  ((foldr (lambda (x acc) (compose acc (curry f x))) identity xs) init))