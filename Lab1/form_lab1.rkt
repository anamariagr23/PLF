#lang racket
(define (m a b)
  (max a b)
  )
(m 5 9)

(define (divizibil x)
  (if (eq? (remainder x 6) 0) (quote "divizibil cu 6")
      (if (eq? (remainder x 2) 0) (quote "divizibil cu 2")
          (if (eq? (remainder x 3) 0) (quote "divizibil cu 3") (quote "nedivizibil cu 2, 3 , 6"))))
  )

(divizibil 9)