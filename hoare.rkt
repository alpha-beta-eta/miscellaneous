#lang racket
;concrete syntax

;<cmd>     ::= <item>
;           |  (seq <item> <item>*)
;<item>    ::= (:= <var> <exp>)
;           |  <guarded>
;<guarded> ::= (if <test> <cmd> <cmd>)
;           |  (while <test> <cmd>)

;<cmd/a>     ::= <item/a>
;             |  (seq <item/a> <step/a>*)
;<item/a>    ::= (:= <var> <exp>)
;             |  <guarded/a>
;<step/a>    ::= (:= <var> <exp>)
;             |  (pre <assertion> <guarded/a>)
;<guarded/a> ::= (if <test> <cmd/a> <cmd/a>)
;             |  (while <test> (inv <assertion>) <cmd/a>)

;<spec> ::= (Hoare <assertion> <cmd/a> <assertion>)

(define (subst Q E V)
  (cond ((pair? Q) (cons (subst (car Q) E V)
                         (subst (cdr Q) E V)))
        ((eq? Q V) E)
        (else Q)))
(define (:= V E)
  (case-lambda
    ((P Q) (list `(=> ,P ,(subst Q E V))))
    ((Q) (values (subst Q E V) '()))))
(define ((IF S C1 C2) P Q)
  (append (C1 `(and ,P ,S) Q)
          (C2 `(and ,P (not ,S)) Q)))
(define ((WHILE S R C) P Q)
  `((=> ,P ,R)
    (=> (and ,R (not ,S)) ,Q)
    . ,(C `(and ,R ,S) R)))
(define ((PRE R C) Q)
  (values R (C R Q)))
(define ((SEQ C . C*) P Q)
  (let iter ((RC* (reverse C*))
             (R Q) (VC '()))
    (if (null? RC*)
        (append (C P R) VC)
        (let-values (((R VC0) ((car RC*) R)))
          (iter (cdr RC*) R (append VC0 VC))))))
(define (Hoare P C Q) (C P Q))

(Hoare
 '(and (= X x) (= Y y))
 (SEQ (:= 'R 'X)
      (:= 'X 'Y)
      (:= 'Y 'R))
 '(and (= Y x) (= X y)))

(Hoare
 #t
 (SEQ (:= 'R 'X)
      (:= 'Q 0)
      (PRE
       '(and (= R X) (= Q 0))
       (WHILE '(<= Y R)
              '(= X (+ R (* Y Q)))
              (SEQ (:= 'R '(- R Y))
                   (:= 'Q '(+ Q 1))))))
 '(and (= X (+ R (* Y Q))) (< R Y)))

(Hoare
 '(<= X Y)
 (IF '(<= X Y)
     (SEQ (:= 'MIN 'X)
          (:= 'MAX 'Y))
     (SEQ (:= 'MIN 'Y)
          (:= 'MAX 'X)))
 '(and (= MIN X) (= MAX Y)))
