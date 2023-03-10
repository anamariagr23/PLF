#lang racket
(+ 4 6 )
(+ 2 (* 3 4))
(/ 3 4)
(max 1 2 3)
(expt 2 4)
(sqrt 25)
(expt 25 (/ 1 2))
(/ 1.0 2)
(abs -5)
pi
; remainder = rest
(remainder 14 5 )
; quotinet rest
; ' defineste o lista

(define b 4)
b
'(+ 2 5)

; let - locale , define - globale
(let ((x 2) (y 3)) (+ x y))

;(cond ((< x 1) 1)
    ;  ((and (> x 1)
  ;          (< x 3) 2 )
      ; (#t 3))
    ;  )

(cons 2 3)


; definire de functie care face suma
(define (foo a b c)
  (+ a b c))
(foo 2 3 5)


;tema ex 1 nr par diviziil cu 7