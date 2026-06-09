(define/contract (pivot-array nums pivot)
  (-> (listof exact-integer?) exact-integer? (listof exact-integer?))
  (let ([lt (filter (lambda (x) (< x pivot)) nums)]
        [gt (filter (lambda (x) (> x pivot)) nums)]
        [eq (filter (lambda (x) (= x pivot)) nums)])
    (append lt eq gt)))
