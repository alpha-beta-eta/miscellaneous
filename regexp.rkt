#lang racket
(require "match.rkt")
;<exp> ::= zero
;       |  one
;       |  <char>
;       |  (+ <exp> <exp>) : union
;       |  (* <exp> <exp>) : concatenation
;       |  (k <exp>) : kleene star
(define (+ e1 e2)
  (cond
    ((eq? e1 'zero) e2)
    ((eq? e2 'zero) e1)
    (else `(+ ,e1 ,e2))))
(define (* e1 e2)
  (cond
    ((eq? e1 'zero) 'zero)
    ((eq? e2 'zero) 'zero)
    ((eq? e1 'one) e2)
    ((eq? e2 'one) e1)
    (else `(* ,e1 ,e2))))
(define (k exp)
  (cond
    ((eq? exp 'zero) 'one)
    ((eq? exp 'one) 'one)
    (else `(k ,exp))))
(define (delta? exp)
  (match exp
    (zero #f)
    (one #t)
    (,c (guard (char? c)) #f)
    ((+ ,e1 ,e2)
     (or (delta? e1) (delta? e2)))
    ((* ,e1 ,e2)
     (and (delta? e1) (delta? e2)))
    ((k ,e) #t)))
(define (empty? exp)
  (match exp
    (zero #t)
    (one #f)
    (,c (guard (char? c)) #f)
    ((+ ,e1 ,e2)
     (and (empty? e1) (empty? e2)))
    ((* ,e1 ,e2)
     (or (empty? e1) (empty? e2)))
    ((k ,e) #f)))
(define (D exp c)
  (match exp
    (zero 'zero)
    (one 'zero)
    (,c^
     (guard (char? c^))
     (if (char=? c^ c) 'one 'zero))
    ((+ ,e1 ,e2) (+ (D e1 c) (D e2 c)))
    ((* ,e1 ,e2)
     (if (delta? e1)
         (+ (* (D e1 c) e2) (D e2 c))
         (* (D e1 c) e2)))
    ((k ,e) (* (D e c) (k e)))))
(define (in? str exp)
  (define l (string-length str))
  (let iter ((i 0) (exp exp))
    (cond
      ((= i l) (delta? exp))
      ((empty? exp) #f)
      (else
       (iter (add1 i)
             (D exp (string-ref str i)))))))
;(define L
;  (* #\c (* (+ #\a #\d) (* (k (+ #\a #\d)) #\r))))
;(in? "cadar" L)