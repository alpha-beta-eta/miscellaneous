#lang racket
(require "vector.rkt")
(define (shuffle! v)
  (define l (vector-length v))
  (unless (< l 2)
    (let iter ((i l))
      (unless (= i 1)
        (vector-swap! v (random i) (- i 1))
        (iter (- i 1))))))