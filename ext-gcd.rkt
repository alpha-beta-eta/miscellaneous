#lang racket
(provide ext-gcd)
(define (ext-gcd a b)
  (if (= b 0)
      (values a 1 0)
      (let ((q (quotient a b))
            (r (remainder a b)))
        (let-values (((d m n) (ext-gcd b r)))
          (values d n (- m (* q n)))))))