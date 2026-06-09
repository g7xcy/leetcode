#lang racket

(define/contract (vowel-strings words left right)
  (-> (listof string?) exact-integer? exact-integer? exact-integer?)
  (let ([is-vowel?
         (lambda (c) (or
                       (eq? c #\a)
                       (eq? c #\e)
                       (eq? c #\i)
                       (eq? c #\o)
                       (eq? c #\u)))])
    (length
      (filter
        (lambda (s) (and
                      (is-vowel? (string-ref s 0))
                      (is-vowel? (string-ref s (sub1 (string-length s))))))
          (drop (take words (add1 right)) left)))))
