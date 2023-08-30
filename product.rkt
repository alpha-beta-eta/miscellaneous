#lang racket
(provide product)
(define (product . lst*)
  (if (null? lst*)
      '(())
      (append-map
       (lambda (d)
         (map (lambda (a) (cons a d))
              (car lst*)))
       (apply product (cdr lst*)))))