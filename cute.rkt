#lang racket
;<shape> ::= <integer>
;         |  (<shape>*)
(struct layout
  (shape stride)
  #:transparent)
(define (->shape x)
  (cond ((layout? x)
         (layout-shape x))
        (else x)))
(define (size0 s)
  (if (integer? s)
      s
      (apply * (map size0 s))))
(define (size x)
  (size0 (->shape x)))
(define (rank0 s)
  (if (integer? s)
      1
      (length s)))
(define (rank x)
  (rank0 (->shape x)))
(define (depth0 s)
  (if (integer? s)
      0
      (add1 (apply max 0 (map depth0 s)))))
(define (depth x)
  (depth0 (->shape x)))
(define (get0 s i)
  (cond ((integer? s)
         (if (= i 0)
             s
             (error 'get0 "invalid mode: ~s ~s" s i)))
        (else (list-ref s i))))
(define (get x i)
  (cond ((layout? x)
         (layout (get0 (layout-shape x) i)
                 (get0 (layout-stride x) i)))
        (else (get0 x i))))
