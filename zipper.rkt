#lang racket
(struct path (left up right) #:transparent)
(struct zipper (focus context) #:transparent)
(define (goLeft z)
  (match z
    ((zipper focus context)
     (match context
       ('top (error 'goLeft "left of top"))
       ((path (cons l left) up right)
        (zipper l (path left up (cons focus right))))
       (_ (error 'goLeft "left of first"))))))
(define (goRight z)
  (match z
    ((zipper focus context)
     (match context
       ('top (error 'goRight "right of top"))
       ((path left up (cons r right))
        (zipper r (path (cons focus left) up right)))
       (_ (error 'goRight "right of last"))))))
(define (goUp z)
  (match z
    ((zipper focus context)
     (match context
       ('top (error 'goUp "up of top"))
       ((path left up right)
        (zipper (append (reverse left)
                        (cons focus right))
                up))))))
(define (goDown z)
  (match z
    ((zipper focus context)
     (match focus
       ((cons x x*)
        (zipper x (path '() context x*)))
       ('() (error 'goDown "down of empty"))
       (_ (error 'goDown "down of item"))))))
(define (change z x)
  (match z
    ((zipper focus context)
     (zipper x context))))
(define (insertLeft z l)
  (match z
    ((zipper focus context)
     (match context
       ('top (error 'insertLeft "insert of top"))
       ((path left up right)
        (zipper focus (path (cons l left) up right)))))))
(define (insertRight z r)
  (match z
    ((zipper focus context)
     (match context
       ('top (error 'insertRight "insert of top"))
       ((path left up right)
        (zipper focus (path left up (cons r right))))))))
(define (insertDown z x)
  (match z
    ((zipper focus context)
     (if (or (pair? focus) (null? focus))
         (zipper x (path '() context focus))
         (error 'insertDown "down of item")))))
(define (delete z)
  (match z
    ((zipper focus context)
     (match context
       ('top (error 'delete "delete of top"))
       ((path left up (cons r right))
        (zipper r (path left up right)))
       ((path (cons l left) up right)
        (zipper l (path left up right)))
       ((path '() up '())
        (zipper '() up))))))
