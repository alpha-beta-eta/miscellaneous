#lang racket
(provide Sxml)
(require "match.rkt")
;<sxml> ::= <string>
;        |  (<symbol> (<attr>*) <sxml>*)
;<attr> ::= (<symbol> <string>)
(define (Attr* attr*)
  (for-each
   (lambda (attr)
     (printf " ~s=~s" (car attr) (cadr attr)))
   attr*))
(define (Sxml sxml)
  (match sxml
    ((,tag ,attr* . ,sxml*)
     (printf "<~s" tag)
     (Attr* attr*)
     (printf ">")
     (for-each Sxml sxml*)
     (printf "</~s>" tag))
    (,str
     (guard (string? str))
     (printf "~a" str))))