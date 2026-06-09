(define/contract (get-common nums1 nums2)
  (-> (listof exact-integer?) (listof exact-integer?) exact-integer?)
  (match* (nums1 nums2)
    [('() _) -1]
    [(_ '()) -1]
    [((cons x xs) (cons y ys))
     (cond
       [(= x y) x]
       [(< x y) (get-common xs nums2)]
       [else (get-common nums1 ys)])]))
