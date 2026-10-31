#lang racket
(struct monoid
  (type zero op)
  #:transparent)
(struct collection_monoid
  (type zero op idx gen opgen)
  #:transparent)
(struct observable_monoid
  (type zero op idx gen opgen obs observe)
  #:transparent)
(define (fold-left f i l)
  (if (null? l)
      i
      (fold-left f (f i (car l)) (cdr l))))
(define (monoid->collection_monoid m)
  (match-define (monoid type zero op) m)
  (collection_monoid
   type zero op type identity op))
(define (collection_monoid->observable_monoid m)
  (match-define
    (collection_monoid
     type zero op idx gen opgen)
    m)
  (observable_monoid
   type zero op idx gen opgen type identity))
(define (product_monoid m1 m2)
  (match-define
    (collection_monoid
     type1 zero1 op1 idx1 gen1 opgen1)
    m1)
  (match-define
    (collection_monoid
     type2 zero2 op2 idx2 gen2 opgen2)
    m2)
  (unless (equal? idx1 idx2)
    (error 'product_monoid "type mismatch: ~s ~s"
           idx1 idx2))
  (collection_monoid
   `(* ,type1 ,type2)
   (cons zero1 zero2)
   (λ (p1 p2)
     (cons (op1 (car p1) (car p2))
           (op2 (cdr p1) (cdr p2))))
   idx1 (λ (i) (cons (gen1 i) (gen2 i)))
   (λ (p i)
     (cons (opgen1 (car p) i)
           (opgen2 (cdr p) i)))))
(define (attach x* x)
  (append x* (list x)))
(define (list_monoid type)
  (collection_monoid
   `(list ,type) '() append type list attach))
(define integer_list (list_monoid 'integer))
(define sum_monoid
  (monoid->collection_monoid
   (monoid 'integer 0 +)))
(define len_monoid
  (collection_monoid
   'integer 0 + 'integer (const 1)
   (λ (x i) (+ x 1))))
(define (map_reduce m)
  (match-define
    (collection_monoid
     type zero op idx gen opgen)
    m)
  (lambda (idx0 lst)
    (unless (equal? idx idx0)
      (error 'map_reduce "mismatch types: ~s ~s"
             idx idx0))
    (fold-left opgen zero lst)))
(define sum (map_reduce sum_monoid))
(define len (map_reduce len_monoid))
(define sum*len_monoid
  (product_monoid sum_monoid len_monoid))
(define sum*len
  (map_reduce sum*len_monoid))
(define average_observable_monoid
  (match sum*len_monoid
    ((collection_monoid
      type zero op idx gen opgen)
     (observable_monoid
      type zero op idx gen opgen
      'number (lambda (p) (/ (car p) (cdr p)))))))
(define (map_reduce_conj m)
  (match-define
    (observable_monoid
     type zero op idx gen opgen obs observe)
    m)
  (lambda (idx0 lst)
    (unless (equal? idx idx0)
      (error 'map_reduce_conj "mismatch types: ~s ~s"
             idx idx0))
    (observe (fold-left opgen zero lst))))
(define average
  (map_reduce_conj
   average_observable_monoid))
(struct ArgMax (max len offset) #:transparent)
(define argmax_monoid
  (let ((op (λ (x1 x2)
              (match* (x1 x2)
                (((ArgMax m1 l1 o1)
                  (ArgMax m2 l2 o2))
                 (cond ((< m1 m2)
                        (ArgMax m2 (+ l1 l2) (+ l1 o2)))
                       (else
                        (ArgMax m1 (+ l1 l2) o1))))))))
    (collection_monoid
     'ArgMax (ArgMax -inf.0 0 #f) op
     'integer (λ (i) (ArgMax i 1 0))
     (λ (x i) (op x (ArgMax i 1 0))))))
(define argmax (map_reduce argmax_monoid))
