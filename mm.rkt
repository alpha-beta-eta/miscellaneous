#lang racket
(define (mref m i j)
  (vector-ref (vector-ref m i) j))
(define (mset! m i j x)
  (vector-set! (vector-ref m i) j x))
(define (nrows m)
  (vector-length m))
(define (ncols m)
  (vector-length (vector-ref m 0)))
(define (build-matrix i j p)
  (build-vector
   i (lambda (i)
       (build-vector
        j (lambda (j) (p i j))))))
(define make-matrix
  (case-lambda
    ((i j x) (build-matrix
              i j (lambda (i j) x)))
    ((i j) (make-matrix i j 0))))
(define ((magic C A B) i k j)
  `(mset! ,C ,i ,j
          (+ (mref ,C ,i ,j)
             (* (mref ,A ,i ,k)
                (mref ,B ,k ,j)))))
(define (mm A B)
  (define I (nrows A))
  (define J (ncols B))
  (define K (ncols A))
  (define K^ (nrows B))
  (unless (= K K^)
    (error 'mm "~s and ~s mismatch" A B))
  (define m (magic 'C 'A 'B))
  `(let ((A ,A)
         (B ,B)
         (C (make-matrix ,I ,J 0)))
     ,@(for*/list ([i (in-range I)]
                   [k (in-range K)]
                   [j (in-range J)])
         (m i k j))
     C))
(mm #(#(1 2)
      #(3 4))
    #(#(5 6)
      #(7 8)))
