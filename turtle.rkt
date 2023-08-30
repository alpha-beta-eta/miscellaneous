#lang racket
(require graphics)
(open-graphics)
(define canvas
  (open-viewport "canvas" 1024 1024))
(define line
  (draw-line canvas))
(define deg->rad
  (let ((c (/ pi 180)))
    (lambda (x)
      (* c x))))
(define (cosd x)
  (cos (deg->rad x)))
(define (sind x)
  (sin (deg->rad x)))
(define (make-turtle name)
  (define p
    (make-posn 512 512))
  (define d -90)
  (define c (cosd d))
  (define s (sind d))
  (define (turn x)
    (set! d (- d x))
    (set! c (cosd d))
    (set! s (sind d)))
  (define (move x)
    (define p^
      (make-posn
       (+ (posn-x p) (* c x))
       (+ (posn-y p) (* s x))))
    (line p p^)
    (set! p p^))
  (lambda (msg . arg*)
    (case msg
      ((turn) (apply turn arg*))
      ((move) (apply move arg*))
      (else (error name "unknown command ~s" msg)))))
(define-syntax for
  (syntax-rules (= upto downto)
    ((_ i = a upto b body* ...)
     (let ((bv b))
       (let iter ((i a))
         (unless (> i bv)
           body* ...
           (iter (+ i 1))))))
    ((_ i = a downto b body* ...)
     (let ((bv b))
       (let iter ((i a))
         (unless (< i bv)
           body* ...
           (iter (- i 1))))))))
(define t0 (make-turtle 't0))
(define (move0 x)
  (t0 'move x))
(define (turn0 x)
  (t0 'turn x))
