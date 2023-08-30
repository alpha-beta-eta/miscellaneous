#lang racket
(provide fact)
(define (fact n)
  (let iter ((n n) (r 1))
    (if (= n 0)
        r
        (iter (- n 1) (* n r)))))