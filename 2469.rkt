#lang racket

(define/contract (convert-temperature celsius)
  (-> flonum? (listof flonum?))
  (list (+ celsius 273.15) (+ (* celsius 1.8) 32.0)))
