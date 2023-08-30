#lang racket
(define zero
  (lambda (f)
    (lambda (x)
      x)))
(define one
  (lambda (f)
    (lambda (x)
      (f x))))
(define true
  (lambda (x)
    (lambda (y)
      x)))
(define false
  (lambda (x)
    (lambda (y)
      y)))
(define cons
  (lambda (x)
    (lambda (y)
      (lambda (m)
        ((m x) y)))))
(define car
  (lambda (p)
    (p true)))
(define cdr
  (lambda (p)
    (p false)))
(define succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))
(define pred
  (lambda (n)
    (car
     ((n (lambda (p)
           ((cons (cdr p)) (succ (cdr p)))))
      ((cons zero) zero)))))
(define (inspect n)
  ((n add1) 0))