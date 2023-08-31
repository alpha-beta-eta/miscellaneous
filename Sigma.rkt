#lang racket
(provide Sigma)
(define (Sigma f a b)
  (if (> a b)
      0
      (+ (f a)
         (Sigma f (+ a 1) b))))