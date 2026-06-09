(define/contract (to-lower-case s)
  (-> string? string?)
  (string-downcase s))

; or
(define/contract (to-lower-case s)
  (-> string? string?)
  (list->string (map (lambda (c) (if (and (char>=? c #\A) (char<=? c #\Z)) (integer->char (+ (char->integer c) 32)) c)) (string->list s))))