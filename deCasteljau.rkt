#lang racket
(define (make-pt x y) (vector 'pt x y))
(define (pt-x pt) (vector-ref pt 1))
(define (pt-y pt) (vector-ref pt 2))
(define (pt+ p1 p2)
  (make-pt
   (+ (pt-x p1) (pt-x p2))
   (+ (pt-y p1) (pt-y p2))))
(define (pt* t p)
  (make-pt
   (* t (pt-x p))
   (* t (pt-y p))))
(define (section p1 p2 t)
  (pt+ (pt* (- 1 t) p1)
       (pt* t p2)))
(define (dC p* t)
  (let r ((p1 (car p*))
          (p2 (cadr p*))
          (p* (cddr p*)))
    (if (null? p*)
        (list (section p1 p2 t))
        (cons (section p1 p2 t)
              (r p2 (car p*) (cdr p*))))))
(define (deCasteljau p* t)
  (if (null? (cddr p*))
      (let ((p1 (car p*))
            (p2 (cadr p*)))
        (section p1 p2 t))
      (deCasteljau (dC p* t) t)))