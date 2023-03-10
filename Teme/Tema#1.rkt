#lang racket

;;; Groza Anamaria
;;; Data: 02.03.2023
;;; Tema1: Familiarizare cu limbajul Racket

;;; P#1 Definiti o functie even-nb-divisible-by-7 care se comporta astfel:

;;; ( even-nb-divisible-by-7 7 )
;;; #t
;;; ( even-nb-divisible-by-7 0 )
;;; #t
;;; ( even-nb-divisible-by-7 10 )
;;; #t
;;; ( even-nb-divisible-by-7 11 )
;;; #f
;;; ( even-nb-divisible-by-7 14 )
;;; #t

;;; Verific daca am primit ca parametru un intreg
;;; In cazul in care numarul primit ca parametru este divizibil cu 7, functia returneaza true
;;; in exemplul cu 10 si 0 ultima verificam daca numarul este par, adica divizibil cu 2
;;; daca nu e divizibil cu 7 si nicici cu 2 , atunci se returneaza false


(define (even-nb-divisible-by-7 a) ; am definit functia
  (if (and(integer? a)(>= 0)) ; Verific daca am primit ca parametru un numar intreg
  (cond ((or (eq? (remainder a 7) 0) (eq? (remainder a 2) 0)) #t) ; verific daca se divide cu 7 sau 2
        (#t #f) )
  (quote "argumentul nu este un numar intreg")
  )

)

(quote "Ex1:")
(even-nb-divisible-by-7 77) ;#t
(even-nb-divisible-by-7 10) ;#t
(even-nb-divisible-by-7 0) ;#t
(even-nb-divisible-by-7 11) ;f
(even-nb-divisible-by-7 4.9) ;f

;;; P#2 Definiti o functie care primeste doi parametri si returneaza true daca prima
;;; lista e mai lunga decat a doua.

;;; Verific daca argumentele primite sunt liste
;;; compar lungimile listelor si returnez true daca prima lista are lungime mai mare , false in caz contrar


(define (longer-listp list1 list2)
  
  (if (and (list? list1) (list? list2)) ; Verific daca argumentele primite sunt liste
      (if (> (length list1) (length list2)) #t #f) ;compar lungimile
      (quote "argumentele nu sunt liste"))
)

(quote "Ex2:")
(longer-listp '(1 2 3) '(5 6)) ;#t
(longer-listp '(1 2 3) '(5 6 9 10 11)) ;#f
(longer-listp 9 7 ) ;"argumentele nu sunt liste"


;;; P#3 Utilizati car, cdr, si combinatii ale lor pentru a returna:

;;; LISTA de input: (A (L K (P O) ) I )
;;; rezultat: O
;;; respectiv (O)

(quote "Ex3:")

(car(cdr(car(cdr(cdr(car(cdr '(A (L K (P O)) I))))))))
(cdr(car(cdr(cdr(car(cdr '(A (L K (P O)) I)))))))

;;; LISTA de input: (A (( L K) (P O)) I)
;;; rezultat: O
;;; respectiv (k)

(car(cdr(car(cdr(car(cdr'(A (( L K) (P O)) I)))))))
(cdr(car(car(cdr '(A (( L K) (P O)) I)))))

;;; LISTA de input: (A (B C . D) (HELLO TODAY) I AM HERE)
;;; rezultat: HELLO
;;; respectiv AM

(car(car(cdr(cdr ' (A (B C . D) (HELLO TODAY) I AM HERE)))))
(car(cdr(cdr(cdr(cdr '(A (B C . D) (HELLO TODAY) I AM HERE))))))

;;; P#4 Definiti o funcție care primește 2 perechi de numere (f1, f2) si (f3, f4),
;;; desemnate de membrul 1 si membrul 2 al acestora, precum și un operator (unul din operatorii +, -, * sau /).
;;; (define (calc op f1 f2 f3 f4) ;; )

;;; Funcția va returna o lista, desemnând o pereche (f5,f6) , rezultatul operației op aplicată celor 2 perechi.

(quote "Ex4:")

(define (calc op f1 f2 f3 f4 )
  (if (and (number? f1) (number? f2) (number? f3) (number? f4)) ;verific daca argumentele primite sunt numere
      (cond ((equal? op '+) (list (+ f1 f3) (+ f2 f4)))
            ((equal? op '-) (list (- f1 f3) (- f2 f4)))
            ((equal? op '*) (list (* f1 f3) (* f2 f4)))
            ((equal? op '/) (if (or (= f3 0) (= f4 0)) "Numitorul nu are voie sa fie 0"
                                (list (/ f1 f3) (/ f2 f4))))
            (#t "Op nu este operator valid"))
      "f1, f2 , f3 sau f4 nu  numar")
  )

(calc '+ 1 2 3 4) ;(4,6)
(calc 'X 1 2 3 4)

;;; P#5 Definiti o funcție care calculează rezultatul expresiei de mai jos.
;;; Tratati cu grija toate cazurile posibile, inclusiv când x ar putea sa nu fie număr!

;;;	E(x) = 2 * x, daca x este in [0,5]
;;;	E(x) = sqrt(x), daca x este in (5, 7]
;;;	E(x) = 20/x, altfel

(quote "Ex5:")

(define (expresie x)
  (if (number? x)
  (cond ((and (>= x 0) (<= x 5)) (* 2 x))
        ((and (> x 5) (<= x 7)) (sqrt x))
        (#t (/ 20 x)))
  "Argumentul nu este un numar")
  )

(expresie '?)
(expresie 7)

