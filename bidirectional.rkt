#lang racket
(struct Some (v) #:transparent)
(struct None () #:transparent)
;term
(struct Hyp (P) #:transparent)
(struct Cons (a d) #:transparent)
(struct Car (p) #:transparent)
(struct Cdr (p) #:transparent)
(struct Inl (a) #:transparent)
(struct Inr (b) #:transparent)
(struct Case (t a b) #:transparent)
(struct Lam (F) #:transparent)
(struct App (a b) #:transparent)
(struct Unit () #:transparent)
(struct Abort (a) #:transparent)
;prop
(struct And (P Q) #:transparent)
(struct Or (P Q) #:transparent)
(struct Imp (P Q) #:transparent)
(struct True () #:transparent)
(struct False () #:transparent)
(struct Var (x) #:transparent)
(define (Not P)
  (Imp P (False)))

(define check
  (match-lambda**
    (((Cons M N) (And A B))
     (and (check M A) (check N B)))
    (((Unit) (True)) #t)
    (((Inl M) (Or A B))
     (check M A))
    (((Inr N) (Or A B))
     (check N B))
    (((Case M F G) P)
     (match (synth M)
       ((Some (Or A B))
        (and (check (F (Hyp A)) P)
             (check (G (Hyp B)) P)))
       (_ #f)))
    (((Lam F) (Imp A B))
     (check (F (Hyp A)) B))
    (((Abort M) P)
     (match (synth M)
       ((Some (False)) #t)
       (_ #f)))
    ((M (Var x))
     (match (synth M)
       ((Some (Var x^)) (eq? x^ x))
       (_ #f)))
    ((M A) #f)))
(define synth
  (match-lambda
    ((Hyp A) (Some A))
    ((Car M) (match (synth M)
               ((Some (And A B)) (Some A))
               (_ (None))))
    ((Cdr M) (match (synth M)
               ((Some (And A B)) (Some B))
               (_ (None))))
    ((App M N) (match (synth M)
                 ((Some (Imp A B))
                  (if (check N A)
                      (Some B)
                      (None)))
                 (_ (None))))
    (M (None))))
#;
(check (Cons (Unit) (Unit))
       (And (True) (True)))
#;
(check
 (Lam (λ (x) (Lam (λ (y) x))))
 (Imp (Var 'A) (Imp (Var 'B) (Var 'A))))
#;
(check
 (Lam (λ (p) (Cons (Car p) (Cdr p))))
 (Imp (And (Var 'A) (Var 'B))
      (And (Var 'A) (Var 'B))))
#;
(check
 (Lam (λ (p) p))
 (Imp (And (Var 'A) (Var 'B))
      (And (Var 'A) (Var 'B))))
#;
(check
 (Lam (λ (p) (Car p)))
 (Imp (And (Var 'A) (Var 'B))
      (Var 'A)))
#;
(check
 (Lam (λ (x) x))
 (Imp (False) (False)))
#;
(check
 (Lam (λ (x) (Abort x)))
 (Imp (False) (False)))
#;
(check
 (Lam (λ (x) (Abort
              (App (Cdr x) (Car x)))))
 (Not (And (Var 'A) (Not (Var 'A)))))
