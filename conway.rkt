#lang racket
(define (build-matrix i j p)
  (build-vector
   i (λ (i)
       (build-vector
        j (λ (j) (p i j))))))
(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j))
(define >>
  (case-lambda
    ((f g) (compose g f))
    ((f g . h*) (apply >> (compose g f) h*))))
(define <<
  (case-lambda
    ((x f) (f x))
    ((x f . g*) (apply << (f x) g*))))
(define (fork f g h)
  (λ x (f (apply g x) (apply h x))))
(struct array (shape indexf) #:transparent)
(define (array->list A)
  (match A
    ((array i indexf)
     (build-list i indexf))))
(define ((Row A) i)
  (match A
    ((array (list _ j) indexf)
     (array j (curry indexf i)))))
(define (Rows A)
  (match A
    ((array (list i _) _)
     (array i (Row A)))))
(define ((Map f) A)
  (match A
    ((array shape indexf)
     (array shape (compose f indexf)))))
(define ((Mapi f) A)
  (match A
    ((array shape indexf)
     (array shape
            (λ indices
              (apply f (apply indexf indices) indices))))))
(define ((For-each f) A)
  (match A
    ((array i indexf)
     (let iter ((x 0))
       (unless (= x i)
         (f (indexf x))
         (iter (+ x 1)))))))
(define (For-each2 f)
  (>> Rows
      (For-each
       (For-each f))))
(define ((Zip-with op) A B)
  (match-define (array shapeA indexfA) A)
  (match-define (array shapeB indexfB) B)
  (unless (equal? shapeA shapeB)
    (error 'Zip-with "different shapes: ~s, ~s"
           shapeA shapeB))
  (array shapeA (fork op indexfA indexfB)))
(define ((Reduce f) A)
  (match A
    ((array i indexf)
     (unless (and (integer? i) (> i 0))
       (error 'Reduce "bad shape: ~s" i))
     (let iter ((x 1) (a (indexf 0)))
       (if (= x i)
           a
           (iter (+ x 1) (f a (indexf x))))))))
(define (Reduce2 f)
  (>> Rows
      (Map (Reduce f))
      (Reduce f)))
(define (Materialize A)
  (match A
    ((array i indexf)
     (define vec (build-vector i indexf))
     (array i (curry vector-ref vec)))))
(define (Materialize2 A)
  (match A
    ((array (list i j) indexf)
     (define mat (build-matrix i j indexf))
     (array (list i j) (curry matrix-ref mat)))))
(define (Array vec)
  (array (vector-length vec)
         (curry vector-ref vec)))
(define ((rho2 i j) A)
  (match A
    ((array k indexf)
     (unless (and (integer? k) (= (* i j) k))
       (error 'rho2 "shape mismatch: ~s, ~s, ~s" i j k))
     (array (list i j)
            (λ (x y)
              (indexf (+ (* x j) y)))))))
(define (Print2 A)
  (define B
    (<< A (Map ~a)
        Materialize2))
  (define max-length
    (<< B (Map string-length)
        (Reduce2 max)))
  (define C
    (<< B (Map (λ (str)
                 (string-append
                  (make-string
                   (- max-length
                      (string-length str))
                   #\space)
                  str)))))
  (match-define (array (list i j) _) A)
  (define border
    (string-join
     (make-list j (make-string max-length #\-))
     "+" #:before-first "+"
     #:after-last "+\n"))
  (<< (Rows C)
      (For-each
       (λ (row)
         (display border)
         (display
          (string-join
           (array->list row)
           "|" #:before-first "|"
           #:after-last "|\n")))))
  (display border))
(define (Zero A)
  (match A
    ((array shape _)
     (array shape (const 0)))))
(define (list- l1 l2)
  (map - l1 l2))
(define (restrict x low high)
  (min (max low x) high))
(define (clamp ref shape)
  (map (lambda (x i)
         (restrict x 0 (- i 1)))
       ref shape))
(define (valid? indices shape)
  (andmap
   (λ (x i) (<= 0 x (- i 1)))
   indices shape))
(define (strategy:set0 shape indexf)
  (λ indices
    (if (valid? indices shape)
        (apply indexf indices)
        0)))
(define (strategy:clamp shape indexf)
  (λ indices
    (apply indexf (clamp indices shape))))
(define strategy:current
  (make-parameter strategy:set0))
(define ((Shift offset) A)
  (match A
    ((array shape indexf)
     (array shape
            (λ indices
              (apply
               ((strategy:current) shape indexf)
               (list- indices offset)))))))
(define (Convolve2 A center)
  (>> (Mapi
       (λ (n . indices)
         (if (zero? n)
             (Zero A)
             (<< A (Map (curry * n))
                 (Shift (list- center indices))))))
      (Reduce2 (Zip-with +))))
(define conway-kernel
  (<< (Array #(1 1 1 1 0 1 1 1 1))
      (rho2 3 3)))
(define (neighbor grid)
  (<< conway-kernel
      (Convolve2 grid '(1 1))))
(define (step grid)
  (Materialize2
   ((Zip-with
     (λ (x n)
       (if (= x 1)
           (if (or (= n 2) (= n 3)) 1 0)
           (if (= n 3) 1 0))))
    grid (neighbor grid))))
