#lang racket
(require racket/date)
(define (current-time)
  (date->string (current-date) #t))
(define-syntax iterate
  (syntax-rules ()
    ((_ i a b body ...)
     (let ((bv b))
       (let iter ((i a))
         (unless (= i bv)
           body ...
           (iter (+ i 1))))))))
(define std-output (current-output-port))
;type-of
(define (type-of obj) (vector-ref obj 0))
;deg->rad
(define (deg->rad deg) (/ (* deg pi) 180))
;vec (and also make-pt, make-color)
(define (make-vec x y z) (vector x y z))
(define make-pt make-vec)
(define make-color make-vec)
(define (vec-x vec) (vector-ref vec 0))
(define (vec-y vec) (vector-ref vec 1))
(define (vec-z vec) (vector-ref vec 2))
(define vec:zero (make-vec 0 0 0))
(define black vec:zero)
(define white (make-color 1 1 1))
(define skyblue (make-vec 0.5 0.7 1.0))
(define (vec+ . u*)
  (apply vector-map + u*))
(define (vec- . u*)
  (apply vector-map - u*))
(define (vec* k u)
  (make-vec (* k (vec-x u))
            (* k (vec-y u))
            (* k (vec-z u))))
(define (vec/ u k)
  (make-vec (/ (vec-x u) k)
            (/ (vec-y u) k)
            (/ (vec-z u) k)))
(define (dot* u v)
  (+ (* (vec-x u) (vec-x v))
     (* (vec-y u) (vec-y v))
     (* (vec-z u) (vec-z v))))
(define (cross u v)
  (make-vec
   (- (* (vec-y u) (vec-z v))
      (* (vec-z u) (vec-y v)))
   (- (* (vec-z u) (vec-x v))
      (* (vec-x u) (vec-z v)))
   (- (* (vec-x u) (vec-y v))
      (* (vec-y u) (vec-x v)))))
(define (wise* u v)
  (make-vec (* (vec-x u) (vec-x v))
            (* (vec-y u) (vec-y v))
            (* (vec-z u) (vec-z v))))
(define (norm u) (sqrt (dot* u u)))
(define (normalize u) (vec/ u (norm u)))
(define (lerp u v t)
  (vec+ (vec* (- 1 t) u) (vec* t v)))
;ray
(define (make-ray o d) (vector 'ray o d))
(define (ray-o ray) (vector-ref ray 1))
(define (ray-d ray) (vector-ref ray 2))
(define (at ray t)
  (vec+ (ray-o ray) (vec* t (ray-d ray))))
;record
(define (make-record t p n f m) (vector 'record t p n f m))
(define (record-t record) (vector-ref record 1))
(define (record-p record) (vector-ref record 2))
(define (record-n record) (vector-ref record 3))
(define (record-f record) (vector-ref record 4))
(define (record-m record) (vector-ref record 5))
(define (build-record d t p n m)
  (if (< (dot* d n) 0)
      (make-record t p n #t m)
      (make-record t p (vec- n) #f m)))
;hit
(define (get-hit obj) (vector-ref obj 1))
(define (hit obj ray t_min t_max)
  ((get-hit obj) obj ray t_min t_max))
(define (hit* world ray t_min t_max)
  (let iter ((obj* world) (hit? #f) (record #f) (t_max t_max))
    (if (null? obj*)
        (and hit? record)
        (let ((r (hit (car obj*) ray t_min t_max)))
          (if r
              (iter (cdr obj*) #t r (record-t r))
              (iter (cdr obj*) hit? record t_max))))))
;random
(define (near-zero? u)
  (and (< (abs (vec-x u)) 1e-6)
       (< (abs (vec-y u)) 1e-6)
       (< (abs (vec-z u)) 1e-6)))
(define (rand min max)
  (+ (* (- max min) (random)) min))
(define (rand^) (rand -1 1))
(define (rand-in-sphere)
  (let ((v (make-vec (rand^) (rand^) (rand^))))
    (if (< (dot* v v) 1) v
        (rand-in-sphere))))
(define (rand-unit)
  (let ((v (rand-in-sphere)))
    (if (near-zero? v)
        (rand-unit)
        (normalize v))))
(define (rand-in-disc)
  (let ((v (make-vec (rand^) (rand^) 0)))
    (if (< (dot* v v) 1) v
        (rand-in-disc))))
;write-color
(define (linear->gamma x) (sqrt x))
(define (write-color color)
  (define (cbyte c)
    (exact-floor (* c 255.999)))
  (printf "~s ~s ~s\n"
          (cbyte (linear->gamma (vec-x color)))
          (cbyte (linear->gamma (vec-y color)))
          (cbyte (linear->gamma (vec-z color)))))
;camera
(define (make-camera aspect-ratio image-width samples-per-pixel max-depth
                     vfov lookfrom lookat vup
                     defocus-angle focus-distance)
  (define image-height
    (exact-floor (/ image-width aspect-ratio)))
  (define camera-center lookfrom)
  (define viewport-height
    (* 2 (tan (/ (deg->rad vfov) 2)) focus-distance))
  (define viewport-width
    (/ (* viewport-height image-width) image-height))
  (define w (normalize (vec- lookfrom lookat)))
  (define u (normalize (cross vup w)))
  (define v (cross w u))
  (define viewport-u
    (vec* viewport-width u))
  (define viewport-v
    (vec* (- viewport-height) v))
  (define pixel-delta-u
    (vec/ viewport-u image-width))
  (define pixel-delta-v
    (vec/ viewport-v image-height))
  (define upper-left
    (vec- camera-center (vec* focus-distance w)
          (vec/ viewport-u 2) (vec/ viewport-v 2)))
  (define pixel00
    (vec+ upper-left
          (vec* 0.5 (vec+ pixel-delta-u pixel-delta-v))))
  (define defocus-radius
    (* focus-distance
       (tan (/ (deg->rad defocus-angle) 2))))
  (define defocus-disk-u
    (vec* defocus-radius u))
  (define defocus-disk-v
    (vec* defocus-radius v))
  (define (disc-sample)
    (let ((p (rand-in-disc)))
      (vec+ camera-center
            (vec* (vec-x p) defocus-disk-u)
            (vec* (vec-y p) defocus-disk-v))))
  (define (pixel-sample i j)
    (vec+ pixel00
          (vec* (+ j (rand^)) pixel-delta-u)
          (vec* (+ i (rand^)) pixel-delta-v)))
  (define (get-ray i j)
    (define origin
      (if (= defocus-angle 0)
          camera-center
          (disc-sample)))
    (define direction
      (vec- (pixel-sample i j) origin))
    (make-ray origin direction))
  (define (ray-color ray depth world)
    (if (<= depth 0) black
        (let ((record (hit* world ray 0.001 +inf.0)))
          (if record
              (let-values (((scattered attenuation)
                            ((record-m record) ray record)))
                (if scattered
                    (wise* attenuation
                           (ray-color scattered (- depth 1) world))
                    black))
              (let* ((u (normalize (ray-d ray)))
                     (a (* 0.5 (+ (vec-y u) 1.0))))
                (lerp white skyblue a))))))
  (define (render world)
    (printf "P3\n~s ~s\n255\n"
            image-width image-height)
    (iterate
     i 0 image-height
     (iterate
      j 0 image-width
      (let iter ((n samples-per-pixel) (color-sum black))
        (if (= n 0)
            (write-color (vec/ color-sum samples-per-pixel))
            (iter (- n 1)
                  (vec+ color-sum
                        (ray-color (get-ray i j) max-depth
                                   world))))))
     (fprintf std-output "line ~s has been rendered.\n~s\n"
              i (current-time))))
  render)
;sphere : the first field is fixed to [sphere-hit]
(define (make-sphere h c r m) (vector 'sphere h c r m))
(define (sphere-h sphere) (vector-ref sphere 1))
(define (sphere-c sphere) (vector-ref sphere 2))
(define (sphere-r sphere) (vector-ref sphere 3))
(define (sphere-m sphere) (vector-ref sphere 4))
(define (sphere-hit sphere ray t_min t_max)
  (define center (sphere-c sphere))
  (define r (sphere-r sphere))
  (define m (sphere-m sphere))
  (define o (ray-o ray))
  (define d (ray-d ray))
  (define oc (vec- center o))
  (define a (dot* d d))
  (define h (dot* d oc))
  (define c (- (dot* oc oc) (sqr r)))
  (define discriminant (- (sqr h) (* a c)))
  (and (>= discriminant 0)
       (let* ((sqrtd (sqrt discriminant))
              (root (/ (- h sqrtd) a)))
         (if (< t_min root t_max)
             (let* ((p (at ray root))
                    (n (vec/ (vec- p center) r)))
               (build-record d root p n m))
             (let ((root (/ (+ h sqrtd) a)))
               (and (< t_min root t_max)
                    (let* ((p (at ray root))
                           (n (vec/ (vec- p center) r)))
                      (build-record d root p n m))))))))
(define (build-sphere c r m)
  (make-sphere sphere-hit c r m))
;lambertian
(define (make-lambertian albedo)
  (lambda (ray record)
    (define d (vec+ (record-n record) (rand-unit)))
    (define scattered
      (make-ray (record-p record)
                (if (near-zero? d)
                    (record-n record)
                    d)))
    (values scattered albedo)))
;reflect and refract
(define (reflect v n)
  (vec- v (vec* (* 2 (dot* v n)) n)))
(define (refract uv n η_i/η_t)
  (define cosθ (min (dot* (vec- uv) n) 1.0)) ;tol problem
  (define ⊥ (vec* η_i/η_t (vec+ uv (vec* cosθ n))))
  (define ∥
    (vec* (- (sqrt (abs (- 1 (dot* ⊥ ⊥))))) n))
  (vec+ ⊥ ∥))
;metal
(define make-metal
  (case-lambda
    ((albedo) (lambda (ray record)
                (define reflected
                  (reflect (ray-d ray) (record-n record)))
                (define scattered
                  (make-ray (record-p record) reflected))
                (values scattered albedo)))
    ((albedo fuzz) (lambda (ray record)
                     (define reflected
                       (vec+ (normalize (reflect (ray-d ray) (record-n record)))
                             (vec* fuzz (rand-unit))))
                     (define scattered
                       (make-ray (record-p record) reflected))
                     (values scattered albedo)))))
;dielectric
(define (make-dielectric refraction_index)
  (lambda (ray record)
    (define ri
      (if (record-f record)
          (/ 1.0 refraction_index)
          refraction_index))
    (define u
      (normalize (ray-d ray)))
    (define cosθ (min (dot* (vec- u) (record-n record)) 1.0))
    (define sinθ (sqrt (- 1 (sqr cosθ))))
    (define direction
      (if (or (>= (* ri sinθ) 1.0)
              (> (reflectance cosθ ri) (random)))
          (reflect u (record-n record))
          (refract u (record-n record) ri)))
    (define scattered
      (make-ray (record-p record) direction))
    (values scattered white)))
(define (reflectance cosine ri)
  (define r0
    (sqr (/ (- 1 ri) (+ 1 ri))))
  (+ r0 (* (- 1 r0) (expt (- 1 cosine) 5))))
;camera0
(define camera0
  (make-camera 16/9 ;aspect-ratio
               2560 ;image-width
               16 ;samples-per-pixel
               32 ;max-depth
               20 ;vfov
               (make-vec -2 2 1) ;lookfrom
               (make-vec 0 0 -1) ;lookat
               (make-vec 0 1 0) ;vup
               10.0 ;defocus-angle
               3.4 ;focus-distance
               ))
;world0
(define world0
  (list (build-sphere (make-vec 0 0 -1.2) 0.5 (make-lambertian (make-vec 0.1 0.2 0.5)))
        (build-sphere (make-vec -1 0 -1) 0.5 (make-dielectric 1.5))
        (build-sphere (make-vec -1 0 -1) 0.4 (make-dielectric 2/3))
        (build-sphere (make-vec 1 0 -1) 0.5 (make-metal (make-vec 0.8 0.6 0.2) 0.1))
        (build-sphere (make-vec 0 -100.5 -1) 100 (make-lambertian (make-vec 0.8 0.8 0)))
        ))
(define (render:camera0-world0)
  (with-output-to-file "foo.ppm"
    (lambda () (camera0 world0))
    #:exists 'replace))
(render:camera0-world0)