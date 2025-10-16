#lang racket
(require quickscript base64 racket/gui/base
         (except-in 2htdp/image make-color make-pen))
(define (number->u8-list num)
  (let iter ((rest num) (len 0) (result '()))
    (if (zero? rest)
        (cons len result)
        (let-values (((q r) (quotient/remainder rest 256)))
          (iter q (add1 len) (cons r result))))))
(define (u8-list->number lst k)
  (let iter ((rest (cdr lst)) (counter (car lst)) (result 0))
    (if (= counter 0)
        (k result rest)
        (iter (cdr rest) (sub1 counter)
              (+ (* result 256) (car rest))))))
(define (pack i)
  (base64-encode
   (list->bytes
    (apply append
           (number->u8-list (image-width i))
           (number->u8-list (image-height i))
           (map (lambda (c)
                  (list (color-red c)
                        (color-green c)
                        (color-blue c)
                        (color-alpha c)))
                (image->color-list i))))))
(define (unpack ip)
  (define bl (bytes->list (base64-decode ip)))
  (u8-list->number
   bl (lambda (ln rest)
        (u8-list->number
         rest (lambda (wd payload)
                (define converted
                  (for/list ([rgba (in-slice 4 payload)])
                    (apply color rgba)))
                (color-list->bitmap converted ln wd))))))

(define-script image-to-base64
  #:label "image-to-base64"
  #:output-to #f
  (λ (selection #:definitions ed)
    (define image*
      (let iter ((snip (send ed find-first-snip)) (s* '()))
        (if snip
            (let ((next (send snip next)))
              (if (image? snip)
                  (iter next (cons snip s*))
                  (iter next s*)))
            s*)))
    (send ed begin-edit-sequence)
    (for-each
     (lambda (image)
       (define position
         (send ed get-snip-position image))
       (send ed delete position (add1 position))
       (send ed insert (~s (pack image)) position))
     image*)
    (send ed end-edit-sequence)))

(define-script base64-to-image
  #:label "base64-to-image"
  #:output-to #f
  (λ (selection #:definitions ed)
    (define (literal->bytes lit)
      (read (open-input-string lit)))
    (define (get-snip-string snip)
      (send snip get-text 0 (send snip get-count)))
    (define (get-group-lit group)
      (let iter ((rest group) (str* '()))
        (if (null? rest)
            (apply string-append str*)
            (iter (cdr rest)
                  (cons (get-snip-string (car rest))
                        str*)))))
    (define (unpack-group group)
      (unpack
       (literal->bytes
        (get-group-lit group))))
    (define (delete-self snip)
      (define position
        (send ed get-snip-position snip))
      (define count
        (send snip get-count))
      (send ed delete position (+ position count)))
    (define (get-last-char snip)
      (define count (send snip get-count))
      (string-ref (send snip get-text (sub1 count) count) 0))
    (define (quote-last? snip)
      (char=? (get-last-char snip) #\"))
    (define (collect-bytes-group snip k)
      (let iter ((snip snip) (s* '()))
        (if (quote-last? snip)
            (k (cons snip s*) (send snip next))
            (iter (send snip next)
                  (cons snip s*)))))
    (define bytes-group*
      (let iter ((snip (send ed find-first-snip)) (group* '()))
        (if snip
            (if (is-a? snip string-snip%)
                (let ((prefix (send snip get-text 0 2)))
                  (if (string=? prefix "#\"")
                      (collect-bytes-group
                       snip
                       (lambda (group next)
                         (iter next (cons group group*))))
                      (iter (send snip next) group*)))
                (iter (send snip next) group*))
            group*)))
    (send ed begin-edit-sequence)
    (for-each
     (lambda (group)
       (let iter ((current (car group))
                  (rest (cdr group)))
         (cond ((null? rest)
                (let ((position (send ed get-snip-position current)))
                  (delete-self current)
                  (send ed insert (unpack-group group) position)))
               (else
                (delete-self current)
                (iter (car rest) (cdr rest))))))
     bytes-group*)
    (send ed end-edit-sequence)))