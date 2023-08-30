#lang racket
(provide (all-defined-out))
(define (make-counter init proc)
  (lambda ()
    (set! init (proc init))
    init))
(define fresh-id
  (let ((c (make-counter -1 add1)))
    (lambda (x)
      (string->symbol
       (format "~s.~s" x (c))))))