#lang racket
(provide fib)
(define (fib n)
  (let iter ((a 0) (b 1) (c 0))
    (if (= c n)
        a
        (iter b (+ a b) (+ c 1)))))