#lang racket

(define (factorial n)
  (cond ((< n 0) #f) ((or (eq? n 0) (eq? n 1)) 1)
        (#t (* n (factorial(- n 1))))))

;;; ridicare la putere
;nu merge
#|
(define (x_la_y x y)
  (if (and (eq? y 0) (not(eq? x 0)))
      1
  (if (and (eq? x 0) (> y 0))
      0
  (if (and
|#


;;; lungimea unei liste
(define (len-list l)
  (if (null? l)
      0
  (+ 1 (len-list (cdr l)))))
;
(len-list '( A B C))

;;; numar de numere dintr-o lista
(define (nmb-count l)
  (if(null? l)
     0
  (if (number? (car l))
      (+ 1 (nmb-count (cdr l)))
  (nmb-count (cdr l))))); algoritmul continua daca gaseste un element care nu corespunde in lista
;
(nmb-count '(1 2 3 'a 4))

;;;lista contine elem identice
(define (el-identice l)
  (if ( or (null? l) (eq? (length l) 1))
      #t
  (if (eq? (car l) (car(cdr l)))
      (el-identice (cdr l))
      #f ))) ;cand un element nu coresounde, iese

;;; elementul de pe o anumita pozitie
(define (l-ref lista numar)
  (if (not(and(>= numar 0 ) ( < numar ( - (length lista) 1))))
      "argumente incorecte"
  (if (eq? numar 0) (car lista)
      (l-ref (cdr lista) (- numar 1)))))


(l-ref '(1 2 3 4 5) 4)


;Definiti o functie recursiva care calculeaza suma elementelor unei liste
(define (suma-lista l)
  (if (null? l)
      0
  (+ (car l) (suma-lista (cdr l)))))

(suma-lista '(1 2 3 4 5))

;Definiti o functie recursiva care calculeaza valoarea functiei f(n) = 2 * f(n-1) + 5, pentru n > 0, stiind ca f(1) = 5, pentru n numar natural dat.
(define (calcul-functie n)
  (if (= n 1)
      5
  (+ (* 2 (calcul-functie (- n 1))) 5)))

(calcul-functie 3)

; Definiti o functie recursiva care, pe baza unui numar natural n, construieste lista cu diviziorii acestuia.
;(define (divizori n i)
 ; (if (<= i n)
  ;    (if (= (remainder n i ) 0)

(define (divizori n)
  (define (divizori-aux i)
    (cond ((= i 0) '())
          ((= (remainder n i) 0)
           (cons i (divizori-aux (- i 1))))
          (else (divizori-aux (- i 1)))))
  (divizori-aux n))

(divizori 12)
  
(define (medie list)
  (define (medie-aux list sum count)
    (if (null? list)
        (/ sum count)
        (medie-aux (cdr list) (+ sum (car list)) (+ count 1))))
  (if (null? list)
      0.0
      (medie-aux list 0.0 0)))

(medie '(1 2 3 4 5))
      