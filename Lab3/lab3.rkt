#lang racket

(require racket/trace)

;;1. x^y

(define (^ x y)
  (^-acc x y 1))

(define (^-acc x y p)
  (cond ((eq? x 0)
           (cond ((<= y 0) "operatie imposibila")
                 (#t 0)))
        ((eq? y 0) p)
        ((< y 0) (/ 1 (^-acc x (abs y) p)))
        (#t (^-acc x (- y 1) (* p x)))))

;;2. lungimea unei liste

(define (l ls) ;;la suprafata
  (l-acc ls 0)
)

(define (l-acc ls r) 
  (cond ((null? ls) r)
        (#t (l-acc (cdr ls) (+ r 1)))))

;;3. Numar de numere din lista (rezolvati voi)
(define (length-lista list)
  (length-lista-acc list 0))

(define (length-lista-acc list nr)
  (if (null? list ) nr
      (if (number? (car list))
      (length-lista-acc (cdr list) (+ nr 1))
      (length-lista-acc (cdr list) nr ))))

(trace length-lista-acc)
(length-lista '(1 2 (3 4) 5 a ))

;;4. SKIP -- nu se preteaza folosire de acumulatori aici

;;5. Elemente pe o anume pozitie intr-o lista -- SKIP nu se preteaza acumulatori aici


;;6. Aplatizarea unei liste (splice) utilizand acumulatori

 (define (splice l)
  (define (splice-acc l r)
    (cond ((null? l) r)
          (#t (cond
                ((list? (car l)) (splice-acc (car l) (splice-acc (cdr l) r)))
                (#t (splice-acc (cdr l) (append r (list (car l)))))))))
  (splice-acc l '()))
