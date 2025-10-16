#lang racket
(struct array (shape indexf) #:transparent)
(define (rho A) (array-shape A))
(define (get A) (array-indexf A))
(define (fork f g h)
  (λ x (f (apply g x) (apply h x))))
(define >>
  (case-lambda
    ((f g) (compose g f))
    ((f g . h*) (apply >> (compose g f) h*))))
(define <<
  (case-lambda
    ((x f) (f x))
    ((x f . g*) (apply << (f x) g*))))
(define ((zip-with op) A B)
  (match-define (array shapeA indexfA) A)
  (match-define (array shapeB indexfB) B)
  (unless (equal? shapeA shapeB)
    (error 'zip-with "different shapes: ~s, ~s"
           shapeA shapeB))
  (array shapeA (fork op indexfA indexfB)))
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
(define ((Reduce f) A)
  (match A
    ((array i indexf)
     (unless (and (integer? i) (> i 0))
       (error 'Reduce "bad shape: ~s" i))
     (let iter ((x 1) (a (indexf 0)))
       (if (= x i)
           a
           (iter (+ x 1) (f a (indexf x))))))))
(define ((row A) i)
  (match A
    ((array (list _ j) indexf)
     (array j (curry indexf i)))))
(define (rows A)
  (match A
    ((array (list i _) _)
     (array i (row A)))))
(define (Reduce2 f)
  (>> rows
      (Map (Reduce f))
      (Reduce f)))
(define ((rho2 i j) A)
  (match A
    ((array k indexf)
     (unless (and (integer? k)
                  (= (* i j) k))
       (error 'rho2 "shape mismatch: ~s, ~s, ~s" i j k))
     (array (list i j)
            (λ (x y)
              (indexf (+ (* x j) y)))))))
(define (materialize1 A)
  (match A
    ((array i indexf)
     (define vec (build-vector i indexf))
     (array i (curry vector-ref vec)))))
(define (build-matrix i j p)
  (build-vector
   i (λ (i)
       (build-vector
        j (λ (j) (p i j))))))
(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j))
(define (materialize2 A)
  (match A
    ((array (list i j) indexf)
     (define mat (build-matrix i j indexf))
     (array (list i j) (curry matrix-ref mat)))))
(define (of-array vec)
  (array (vector-length vec)
         (curry vector-ref vec)))
(define (iota n)
  (array n identity))
(define ((for-each1 f) A)
  (match A
    ((array i indexf)
     (let iter ((x 0))
       (unless (= x i)
         (f (indexf x))
         (iter (+ x 1)))))))
(define (for-each2 f)
  (>> rows
      (for-each1
       (for-each1 f))))
(define print-2d
  (>> rows
      (for-each1
       (λ (r)
         (<< r (for-each1
                (curry printf "~s ")))
         (newline)))))
(define ((repeated n) f)
  (if (zero? n)
      identity
      (compose f ((repeated (sub1 n)) f))))
(define (zero-array A)
  (array (rho A) (const 0)))
(define (transpose A)
  (match A
    ((array (list i j) indexf)
     (array (list j i)
            (λ (j i) (indexf i j))))))
(define (duplicate f)
  (λ (x) (f x x)))
(define minmax2
  (>> (Map (duplicate cons))
      (Reduce2
       (match-lambda*
         ((list (cons a b) (cons c d))
          (cons (min a c) (max b d)))))))
(define ((normalize bound) A)
  (match-define (cons min max) (minmax2 A))
  (define range (- max min))
  (if (zero? range)
      (zero-array A)
      (<< A (Map (λ (x)
                   (quotient
                    (* (- x min) bound)
                    range))))))
(struct color (R G B) #:transparent)
(define colors 256)
(define color-max (sub1 colors))
(define cmap:reds
  (array colors (λ (i) (color i 0 0))))
(define cmap:blues
  (array colors (λ (i) (color 0 0 i))))
(define cmap:whites
  (array colors (λ (i) (color i i i))))
(define cmap1
  (array colors
         (λ (i)
           (color
            (min color-max (* i 2))
            i
            (min color-max (* i 2))))))
(define (write-color c)
  (printf "~s ~s ~s\n"
          (color-R c)
          (color-G c)
          (color-B c)))
(define (PPM cmap A)
  (match A
    ((array (list i j) _)
     (printf "P3\n~s ~s\n~s\n" i j color-max)
     (<< A
         (normalize color-max)
         (Map (get (materialize1 cmap)))
         (for-each2 write-color)))))
(define (emitPPM cmap A path)
  (with-output-to-file path
    (λ () (PPM cmap A))
    #:exists 'replace))
(define (rand)
  (- (random 7) 3))
(define (noise A)
  (match A
    ((array shape _)
     (array shape
            (λ (i j)
              (if (and (even? i) (even? j))
                  0
                  (rand)))))))
(define ((mul a) b)
  (exact-round (* a b)))
(define (expander scaler nsf)
  (>> (Map (mul nsf))
      scaler
      (fork (zip-with +) identity noise)
      materialize2))
(define (scale-twice A)
  (match A
    ((array (list i j) indexf)
     (array (list (* i 2) (* j 2))
            (λ (i j)
              (indexf (quotient i 2)
                      (quotient j 2)))))))
(define (list- l1 l2)
  (map - l1 l2))
(define (restrict x low high)
  (min (max low x) high))
(define (clamp ref shape)
  (map (lambda (x i)
         (restrict x 0 (- i 1)))
       ref shape))
(define ((shift2 s) A)
  (match A
    ((array shape indexf)
     (array shape
            (λ indices
              (apply indexf
                     (clamp (list- indices s)
                            shape)))))))
(define (convolve2 A center)
  (>> (Mapi
       (λ (n . indices)
         (if (zero? n)
             (zero-array A)
             (<< A (Map (curry * n))
                 (shift2 (list- center indices))))))
      (Reduce2 (zip-with +))))
(define (interleave2 #:orig orig
                     #:vert vert
                     #:horz horz
                     #:diag diag)
  (define origf (get orig))
  (define vertf (get vert))
  (define horzf (get horz))
  (define diagf (get diag))
  (match orig
    ((array (list i j) _)
     (array (list (* i 2) (* j 2))
            (λ (i j)
              (define a (quotient i 2))
              (define b (quotient j 2))
              (if (even? i)
                  (if (even? j)
                      (origf a b)
                      (horzf a b))
                  (if (even? j)
                      (vertf a b)
                      (diagf a b))))))))
(define (scale-twice-bilinear orig)
  (define vert
    (<< (array '(2 1) (const 1))
        (convolve2 orig '(0 0))
        (Map (curryr quotient 2))))
  (define horz
    (<< (array '(1 2) (const 1))
        (convolve2 orig '(0 0))
        (Map (curryr quotient 2))))
  (define diag
    (<< (array '(2 2) (const 1))
        (convolve2 orig '(0 0))
        (Map (curryr quotient 4))))
  (interleave2
   #:orig orig
   #:vert vert
   #:horz horz
   #:diag diag))
(define bicubic-kernel
  (of-array #(-1 9 9 -1)))
(define (⊗ A B)
  (match-define (array i fA) A)
  (match-define (array j fB) B)
  (array (list i j)
         (λ (i j)
           (* (fA i) (fB j)))))
(define bicubic-bikernel
  (⊗ bicubic-kernel bicubic-kernel))
(define (scale-twice-bc orig)
  (define vert
    (<< bicubic-kernel
        (rho2 4 1)
        (convolve2 orig '(1 0))
        (Map (curryr quotient 16))))
  (define horz
    (<< bicubic-kernel
        (rho2 1 4)
        (convolve2 orig '(0 1))
        (Map (curryr quotient 16))))
  (define diag
    (<< bicubic-bikernel
        (convolve2 orig '(1 1))
        (Map (curryr quotient 256))))
  (interleave2
   #:orig orig
   #:vert vert
   #:horz horz
   #:diag diag))
(define (scale-twice-sd orig)
  (define diamond
    (<< (of-array #(1 2 1 1 2 1))
        (rho2 2 3)))
  (define vert
    (<< diamond
        (convolve2 orig '(0 1))
        (Map (curryr quotient 8))))
  (define horz
    (<< (transpose diamond)
        (convolve2 orig '(1 0))
        (Map (curryr quotient 8))))
  (define diag
    (<< (array '(2 2) (const 1))
        (convolve2 orig '(0 0))
        (Map (curryr quotient 4))))
  (interleave2
   #:orig orig
   #:vert vert
   #:horz horz
   #:diag diag))