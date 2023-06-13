#lang racket

;;;4.1 RANGE  ̧si VALID-RANGE

(define (RANGE ls)
  (define min 100000)
  (define max -100000)
  (define res '()) ; definim lista de rezultat
  (cond [(list? ls)
      (for ([i ls]) ; iteram prin lista primita ca argument
        (if (< i min) (set! min i) (set! min min))
        (if (> i max) (set! max i) (set! max max)))
      
      (set! res (cons max res))
      (set! res (cons min res))
      res]
  [else "Argumentul nu este lista!"]))

( RANGE '( 0 7 8 2 3 -1))

(define (VALID-RANGE ls)
  (define min 100000)
  (define max -100000)
  (define nr 1)
  (define res '()) ; definim lista de rezultat
  (cond [(list? ls)
      (for ([i ls]) ; iteram prin lista primita ca argument
        (cond
          ((not (number? i)) (set! nr 0) (set! nr nr))
          ((< i min) (set! min i) (set! min min))
          ((> i max) (set! max i) (set! max max))))
      
      (set! res (cons max res))
      (set! res (cons min res))
      (if (= nr 1) res "INVALID")]
  [else "Argumentul nu este lista!"]))

(VALID-RANGE '( a 7 8 2 3 -1))


;;;4.4 Problemele din cursul 4 (ultimul slide) ˆın variantele:
;Inversa unei liste - recursiv
(define (revers lst [result null])
  (if (list? lst)
      (cond [(null? lst) result]  ; base case
        [#t (revers (cdr lst)        ; recursive case
                     (cons (car lst) result))])
      (quote "Argumentul nu e lista")))
;(revers '(a b c d e))

;Inversa unei liste - recursiv final

(define (rev lista)
  (rev-aux lista '())
  )
(define (rev-aux lista acc)
   (if (list? lista)
      (cond [(null? lista) acc]  ; base case
        [#t (rev-aux (cdr lista)        ; recursive case cu acumulator
                     (cons (car lista) acc))])
      (quote "Argumentul nu e lista")))
;(rev-aux '(a b c d e))

;Inversa unei liste - iterativ cu do
(define (rev-list l)
   (if (list? l)
   (do ((lista l (cdr lista))
        (solutie '() (cons (car lista) solutie)))
        ((null? lista) solutie)
     )
   "Argumentul nu este lista!"))

;(rev-list '(a b c d e))


;CMMDC - recursiv
(define (cmmdc-rec a b)
  (if (eq? a b)
      a
      (if (> a b)
          (cmmdc-rec (- a b) b)
          (cmmdc-rec a (- b a))
          )
      )
  )

(cmmdc-rec 10 8)

;CMMDC  iterativ cu do
(define (cmmdc-it a b)
  (do ( (x a (if (> x y)
                 (- x y)
                 x
                 ) )
        (y b (if (> y x)
                 (- y x)
                 y
                 ) )
        )
    ( (eq? x y) y )
      )
  )
(cmmdc-it 10 9)



;Suma patratelor numerelor din lista recursiv
(define (sum-patr-rec l)
  (if (empty? l)
      0
      (if (number? (car l))
          (+ (* (car l) (car l)) (sum-patr-rec (cdr l)))
          (sum-patr-rec (cdr l))
          )
      )
  )
(sum-patr-rec '(1 a b c 2))

;Suma patratelor numerelor din lista  recursiv final
(define (sum-patr-rec-fin l [sol 0])
  (if (empty? l)
      sol
      (if (number? (car l))
          (sum-patr-rec-fin (cdr l) (+ (* (car l) (car l)) sol))
          (sum-patr-rec-fin (cdr l) sol)
          )
      )
  )
(sum-patr-rec-fin '(1 a b c 2))

;Suma patratelor numerelor din lista iterativ cu do
(define (sum-patr-it l)
  (do ( (lista l (cdr lista))
        (sol 0 (if (number? (car lista))
                   (+ sol (* (car lista) (car lista)) )
                   sol
                   ) )
        )
    ( (empty? lista) sol )
      )
  )
(sum-patr-it '(1 a b c 2))

;Suma patratelor numerelor din lista superficial iterativ cu for
(define (sum-patr-it-for l)
  (let ( [sol 0] )
    (for ( [el l] )
      (if (number? el)
          (set! sol (+ sol (* el el)))
          (set! sol sol)
          )
      )
    sol
    )
  )
;(sum-patr-it-for '(1 a b c 2))


;Merge-recursiv

> (define (conc l1 l2)
    (if (and (list? l1) (list? l2))
        (cond
          ((eq? 0 (length l1)) l2)
          ((eq? 0 (length l2)) l1)
          (else (cons (car l1) (conc (cdr l1) l2))))
        "Argumentele nu sunt liste!"))




