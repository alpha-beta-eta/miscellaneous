#lang racket
(struct array (shape indexf) #:transparent)
(define (rho A) (array-shape A))
(define (get A) (array-indexf A))
(define >>
  (case-lambda
    ((f g) (compose g f))
    ((f g . h*) (apply >> (compose g f) h*))))
(define <<
  (case-lambda
    ((x f) (f x))
    ((x f . g*) (apply << (f x) g*))))
(define (fork f . g*)
  (λ x (apply f (map (λ (g) (apply g x)) g*))))
(define ((Map f) A . A*)
  (array (rho A)
         (apply fork f (get A) (map get A*))))
(define ((row A) i)
  (match A
    ((array (cons _ j) indexf)
     (array j (curry indexf i)))))
(define (rows A)
  (match A
    ((array (cons i _) _)
     (array i (row A)))))
(define ((Reduce1 f) A)
  (match A
    ((array i indexf)
     (unless (and (integer? i) (> i 0))
       (error 'Reduce "bad shape: ~s" i))
     (let iter ((x 1) (a (indexf 0)))
       (if (= x i)
           a
           (iter (+ x 1) (f a (indexf x))))))))
(define ((Reduce f) A)
  (match A
    ((array (cons _ _) _)
     (<< (rows A)
         (Map (Reduce f))
         (Reduce1 f)))
    (_ ((Reduce1 f) A))))
