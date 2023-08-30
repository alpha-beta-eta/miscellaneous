#lang racket
(require SMathML)
(define-struct vec3
  (x y z)
  #:transparent)
(define-struct vec2
  (x y)
  #:transparent)
(define-struct pt3
  (x y z)
  #:transparent)
(define-struct pt2
  (x y)
  #:transparent)
(define-struct frame
  (o x y z)
  #:transparent)
(define (build-frame ox oy oz
                     xx xy xz
                     yx yy yz
                     zx zy zz)
  (make-frame
   (make-pt3 ox oy oz)
   (make-vec3 xx xy xz)
   (make-vec3 yx yy yz)
   (make-vec3 zx zy zz)))
(define vec3+
  (case-lambda
    ((v1 v2)
     (make-vec3
      (+ (vec3-x v1) (vec3-x v2))
      (+ (vec3-y v1) (vec3-y v2))
      (+ (vec3-z v1) (vec3-z v2))))
    ((v1 v2 . v*)
     (apply vec3+ (vec3+ v1 v2) v*))))
(define (vec3* k v)
  (make-vec3
   (* k (vec3-x v))
   (* k (vec3-y v))
   (* k (vec3-z v))))
(define (pt3+ p v)
  (make-pt3
   (+ (pt3-x p) (vec3-x v))
   (+ (pt3-y p) (vec3-y v))
   (+ (pt3-z p) (vec3-z v))))
(define (pt2+ p v)
  (make-pt2
   (+ (pt2-x p) (vec2-x v))
   (+ (pt2-y p) (vec2-y v))))
(define (painterT f)
  (define o (frame-o f))
  (define x (frame-x f))
  (define y (frame-y f))
  (define z (frame-z f))
  (define ox (pt3-x o))
  (define oy (pt3-y o))
  (define oz (pt3-z o))
  (define xx (vec3-x x))
  (define xy (vec3-y x))
  (define xz (vec3-z x))
  (define yx (vec3-x y))
  (define yy (vec3-y y))
  (define yz (vec3-z y))
  (define zx (vec3-x z))
  (define zy (vec3-y z))
  (define zz (vec3-z z))
  (lambda (painter)
    (lambda (frame)
      (define o0 (frame-o frame))
      (define x0 (frame-x frame))
      (define y0 (frame-y frame))
      (define z0 (frame-z frame))
      (define (lc a b c)
        (vec3+ (vec3* a x0)
               (vec3* b y0)
               (vec3* c z0)))
      (painter
       (make-frame
        (pt3+ o0 (lc ox oy oz))
        (lc xx xy xz)
        (lc yx yy yz)
        (lc zx zy zz))))))
(define (painterT* ox oy oz
                   xx xy xz
                   yx yy yz
                   zx zy zz)
  (painterT
   (build-frame
    ox oy oz
    xx xy xz
    yx yy yz
    zx zy zz)))
(define ((over . p*) f)
  (append-map
   (lambda (p)
     (p f))
   p*))
(define above
  (let ((t1 (painterT* 0 0 0
                       1 0 0
                       0 1 0
                       0 0 1/2))
        (t2 (painterT* 0 0 1/2
                       1 0 0
                       0 1 0
                       0 0 1/2)))
    (lambda (p1 p2)
      (over (t1 p1) (t2 p2)))))
(define beside
  (let ((t1 (painterT* 0 0 0
                       1 0 0
                       0 1/2 0
                       0 0 1))
        (t2 (painterT* 0 1/2 0
                       1 0 0
                       0 1/2 0
                       0 0 1)))
    (lambda (p1 p2)
      (over (t1 p1) (t2 p2)))))
(define before
  (let ((t1 (painterT* 0 0 0
                       1/2 0 0
                       0 1 0
                       0 0 1))
        (t2 (painterT* 1/2 0 0
                       1/2 0 0
                       0 1 0
                       0 0 1)))
    (lambda (p1 p2)
      (over (t1 p1) (t2 p2)))))
(define (scale x y z)
  (painterT*
   0 0 0
   x 0 0
   0 y 0
   0 0 z))
(define (translate x y z)
  (painterT*
   x y z
   1 0 0
   0 1 0
   0 0 1))
(define ((cmap f) p)
  (pt3+ (frame-o f)
        (vec3+ (vec3* (pt3-x p) (frame-x f))
               (vec3* (pt3-y p) (frame-y f))
               (vec3* (pt3-z p) (frame-z f)))))
(define (make-line*-painter lst)
  (lambda (frame)
    (define m (cmap frame))
    (map
     (lambda (pair)
       (match pair
         (((,x1 ,y1 ,z1)
           (,x2 ,y2 ,z2))
          `(line ,(m (make-pt3 x1 y1 z1))
                 ,(m (make-pt3 x2 y2 z2))))))
     lst)))
(define (make-polygon*-painter lst)
  (lambda (frame)
    (define m (cmap frame))
    (map
     (lambda (p*)
       (cons 'polygon
             (map (lambda (p)
                    (match p
                      ((,x1 ,y1 ,z1)
                       (m (make-pt3 x1 y1 z1)))))
                  p*)))
     lst)))
(define base-frame
  (build-frame
   513 -1 -257
   512 0 0
   0 512 0
   0 0 512))
(define (isometric p)
  (define x (pt3-x p))
  (define y (pt3-y p))
  (define z (pt3-z p))
  (make-pt2
   (/ (* (sqrt 3) (- x y)) 2)
   (- (* 1/2 (+ x y)) z)))
(define (n2s n)
  (~r n #:precision 2))
(define ((lerp t) a b)
  (+ a (* t (- b a))))
(define ((biased-random t) n)
  (exact-round ((lerp t) (random n) (- n 1))))
(define (base-convert n b)
  (let iter ((n n) (r '()))
    (if (= n 0)
        r
        (iter (quotient n b)
              (cons (remainder n b) r)))))
(define (dec->hex n)
  (define vec
    #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
          #\A #\B #\C #\D #\E #\F))
  (list->string
   (map (curry vector-ref vec)
        (base-convert n 16))))
(define (random-hex n)
  (dec->hex (random n)))
(define ((biased-random-hex t) n)
  (dec->hex ((biased-random t) n)))
(define (random-color t)
  (format "#~a~a~a"
          ((biased-random-hex t) 256)
          ((biased-random-hex t) 256)
          ((biased-random-hex t) 256)))
(define ((compile-line proj) p1 p2)
  (define P1 (proj p1))
  (define P2 (proj p2))
  (Line #:attr*
        `((x1 ,(n2s (pt2-x P1)))
          (y1 ,(n2s (pt2-y P1)))
          (x2 ,(n2s (pt2-x P2)))
          (y2 ,(n2s (pt2-y P2))))))
(define ((compile-polygon proj) p*)
  (define P* (map proj p*))
  (Polygon #:attr*
           `((points
              ,(apply
                string-append
                (map (lambda (P)
                       (format "~a,~a "
                               (n2s (pt2-x P))
                               (n2s (pt2-y P))))
                     P*)))
             (fill ,(random-color 0.75)))))
(define ((compile-pict proj #:attr* [attr* '()]) pict)
  (keyword-apply
   Svg
   '(#:attr*) (list attr*)
   (map (lambda (instr)
          (match instr
            ((line ,p1 ,p2)
             ((compile-line proj) p1 p2))
            ((polygon . ,p*)
             ((compile-polygon proj) p*))))
        pict)))
(define cube-skeleton
  (make-line*-painter
   '(((0 0 0) (1 0 0))
     ((1 0 0) (1 1 0))
     ((1 1 0) (0 1 0))
     ((0 1 0) (0 0 0))
     ((0 0 0) (0 0 1))
     ((1 0 0) (1 0 1))
     ((1 1 0) (1 1 1))
     ((0 1 0) (0 1 1))
     ((0 0 1) (1 0 1))
     ((1 0 1) (1 1 1))
     ((1 1 1) (0 1 1))
     ((0 1 1) (0 0 1)))))
(define half-cube
  (make-polygon*-painter
   '(((1 0 0)
      (1 1 0)
      (1 1 1)
      (1 0 1))
     ((0 1 0)
      (1 1 0)
      (1 1 1)
      (0 1 1))
     ((0 0 1)
      (1 0 1)
      (1 1 1)
      (0 1 1)))))
(define painter0
  (above
   half-cube
   ((translate 1/4 1/4 0) ((scale 1/2 1/2 1) half-cube))))
(define painter1
  ((scale 1 1 1/2) half-cube))
(define (four p)
  (define q (before p p))
  (beside q q))
(define (double p)
  (above p p))
(define (repeated f n)
  (if (zero? n)
      identity
      (compose f (repeated f (- n 1)))))
(define example-pict
  ((four (four (four painter0)))
   base-frame))
(define example.svg
  ((compile-pict
    isometric
    #:attr*
    '((width "890")
      (height "1028")
      (stroke "black")
      (style "display: block; margin: auto;")))
   example-pict))
(define index.html
  (Prelude
   example.svg
   ))
(emitXml index.html "index.html")
