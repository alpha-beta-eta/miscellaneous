#lang racket
(define (Expr1 add mul)
  (add 1 (mul 2 3)))
(define (Expr2 add mul)
  (mul (add 3 4) (mul 3 4)))
(define (Interp1 Expr)
  (Expr + *))
(define (Interp2 Expr)
  (Expr (λ (x y) `(+ ,x ,y))
        (λ (x y) `(* ,x ,y))))
(define (Interp3 Expr)
  (Expr (λ (x y) (format "(~a + ~a)" x y))
        (λ (x y) (format "(~a * ~a)" x y))))
;; > (Interp1 Expr1)
;; 7
;; > (Interp2 Expr1)
;; '(+ 1 (* 2 3))
;; > (Interp3 Expr1)
;; "(1 + (2 * 3))"
;; > (Interp1 Expr2)
;; 84
;; > (Interp2 Expr2)
;; '(* (+ 3 4) (* 3 4))
;; > (Interp3 Expr2)
;; "((3 + 4) * (3 * 4))"
