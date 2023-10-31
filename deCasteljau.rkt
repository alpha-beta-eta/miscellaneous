#lang racket
(define (make-pt x y) (vector 'pt x y))
(define (pt-x pt) (vector-ref pt 1))
(define (pt-y pt) (vector-ref pt 2))
(define (make-vec x y) (vector 'vec x y))
(define (vec-x vec) (vector-ref vec 1))
(define (vec-y vec) (vector-ref vec 2))
;pt- : pt * pt -> vec
(define (pt- p1 p2)
  (make-vec
   (- (pt-x p1) (pt-x p2))
   (- (pt-y p1) (pt-y p2))))
;pt+ : pt * vec -> pt
(define (pt+ p v)
  (make-pt
   (+ (pt-x p) (vec-x v))
   (+ (pt-y p) (vec-y v))))
;vec* : real * vec -> vec
(define (vec* k v)
  (make-vec
   (* k (vec-x v))
   (* k (vec-y v))))
;lerp : real -> pt * pt -> pt
(define ((lerp t) p1 p2)
  (pt+ p1 (vec* t (pt- p2 p1))))
;deCasteljau : pt* -> real -> pt
(define ((deCasteljau p*) t)
  (let iter ((p* p*))
    (if (null? (cdr p*))
        (car p*)
        (iter (map (lerp t)
                   (drop-right p* 1)
                   (cdr p*))))))