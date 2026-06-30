(define/contract (num-of-strings patterns word)
  (-> (listof string?) string? exact-integer?)
  (count (curry string-contains? word) patterns))
