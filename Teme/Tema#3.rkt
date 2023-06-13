#lang racket
(require racket/trace)

;;; Groza Anamaria
;;; Data: 20.03.2023
;;; Tema2: Familiarizare cu conceptul de recursivitate in Racket, folosind acumulatori


;;;P#2 Definiti o functie care calculeaza inversa unei liste.
#|
> (rev ‘(1 2 3))
‘(3 2 1)
> (rev ‘a)
Eroare! Argumentul nu e o lista
> (rev ‘(a))
‘(a)
|#
(define (rev lista)
  (rev-aux lista '())
  )
(define (rev-aux lista acc)
   (if (list? lista)
      (cond [(null? lista) acc]  ; base case
        [#t (rev-aux (cdr lista)        ; recursive case cu acumulator
                     (cons (car lista) acc))])
      (quote "Argumentul nu e lista")))

(rev '(1 2 3))


;;;P#1 Definiti o functie pentru concatenarea a doua liste. Nu se va folosi append.
;;;> (conc ‘(1 2 3) ‘(4 5 6))
;;;‘(1 2 3 4 5 6)
;;;> (conc ‘(1 2 3) ‘())
;;;‘(1 2 3)
;;;> (conc ‘(1 2 3) ‘a)
;;;Eroare! argumentul 2 nu este o lista

;;;varianta cu acumulatori

(define (conc list1 list2)
  (if (and (list? list1) (list? list2))   ;;;Verific daca argumentele sunt liste
           (cond [(null? list2) list1]    ;;;daca lista 2 e vida atunci returnez lista 1 
                 [else (conc-aux (rev list1) list2 list2)]) ; apelez functia auxiliara cu lista inversata 1 , lista 2 si acc=lista2 
      (quote "Argumentele nu sunt liste")))
;definesc functia auxiliara cu acumulator
(define (conc-aux list1 list2 acc)
  (if (null? list1) acc ; daca lista 1 e vida atunci returnez lista2,care e salvata in acumulator
      (conc-aux (cdr list1) list2 (cons (car list1 ) acc)))) ; altfel iau coada listei1, lista2 si apelez recursiv concatenarea head-ului listei 1 cu acc

(conc '(1 2 3) '(4 5 6))


;;;P#3 Definiti o functie care extrage toate elementele impare dintr-o lista. Nu se va folosi predicatul odd?. Se va parcurge lista in adancime.
;> (odds ‘(1 2 ‘(1 ‘(2 3 4 a)) ‘(4 5 6)))
;‘(1 1 3 5)
;> (odds ‘a)
;Eroare! Argumentul dat nu e o lista
;> (odds ‘())
;‘()

;;; Pentru a parcurge lista in adancime am creat functia flatten ,
;;;care face o lista care contine elementele listei primite si ale sublistelor
;;;vom apela functia flatten in functia odds
;;;in functia odds am verificat daca capul listei e numar, daca este si este si impar l-am adaugat la noua lista
;;; de fapt numerele se vor adauga la revenirea din apelul recursiv

;;;varianta cu acumulatori
;Am lasat functia de flatten neschimbata
(define (flatten lst)
  (cond [(null? lst) null]
        [(pair? (car lst)) (conc (flatten (car lst)) (flatten (cdr lst)))]
        [else (cons (car lst) (flatten (cdr lst)))]))
;definesc functia odds care apeleaza f. auxiliara cu acumulator
(define (odds lst)
  (odds-aux lst '())
  )
;definesc functia auxiliara cu acumulator
(define (odds-aux lst acc)
  (cond ((null? lst) acc) ; cazul de baza, lista e vida
        ((number? (car lst))
         (if (eq? (remainder (car lst) 2) 1)
             (cons (car lst) (odds-aux (cdr lst) acc))
             (odds-aux (cdr lst) acc)))
         (else (odds-aux (cdr lst) acc))) ; se ignora elementele care nu sunt numere
  )

(odds (flatten '(1 2 (1 (2 3 4 a)) (4 5 6))))


;;;P#4 Definiti o functie care extrage elementele de pe pozitiile impare dintr-o lista.
;> (odds-positions ‘(1 2 ‘(a b) 3 4))
;‘(1 ‘(a b) 4)

;;;Dacă lista este vidă, returnăm lista vidă.
;;;Dacă lungimea listei este impară, adăugăm primul element la lista rezultat și apelăm recursiv funcția pe restul listei.
;;;Altfel, apelăm recursiv funcția pe restul listei ignorând primul element.

;;;varianta cu acumulatori
(define (odds-positions lst)
  (odds-positions-acc lst '()))

(define (odds-positions-acc lst acc)
  (cond [(null? lst) acc] ;daca lista e vida
        ;daca lungimea listei e impara vom concatena head-ul listei apelul recursiv pt. coada listei si acumulatorul
        [(eq? (remainder (length lst) 2) 1) (cons (car lst) (odds-positions-acc (cdr lst) acc))]
        [else (odds-positions-acc (cdr lst) acc)]))

(odds-positions '(1 2 (a b) 3 4))

;;;P#5 Definiti o functie pentru calculul recursiv al termenului de rang n, n >= 1, a sirului fibonacci ( f(n) = f(n-1) + f(n-2), pentru n >=3 , iar f(1) = f(2) = 1 )


(define (fib n)
  (if (number? n) ;Am verificat daca argumentul primit e numar
       (if (or (= n 1) (= n 2)) 1
          (fibonacci-acc n 1 1))
   (quote "argumentul nu e numar"))
  )
(define (fibonacci-acc n acc1 acc2)
  (if (= n 2) acc2
      (fibonacci-acc (- n 1) acc2 (+ acc1 acc2))))

(fib 6)

;;;P#6 Definiti o functie pentru calculul recursiv al inversului unui numar.
;> (inv 1234)
;4321
;> (inv ‘a)
;Eroare! argumentul nu e un numar

;;; Am verificat daca argumentul primit este numar
;;; Daca este am calculat inversul dupa formula: invers = (ultima_cifra * 10^(numar_de_cifre - 1)) + inverse(rest_numar_de_cifre)

(define (inv n)
  (if (not (number? n)) "Eroare! argumentul nu e un numar"
      (inv-acc n 0)))

(define (inv-acc n acc)
  (if (number? n)
      (if (= n 0)
          n
          (+ (* (remainder n 10) (expt 10 (- (string-length (number->string n)) 1 )))(inv-acc (quotient n 10) acc)))
      (quote "nu e numar")))

(inv 1234)
(inv 'a)


;;;P#7 Definiti o functie pentru calculul sumei cifrelor unui numar dat

;> (sum 1234)
;10
;> (sum ‘a)
;Eroare! argumentul nu e un numar

(define (sum n)
  (if (number? n) 
      (sum-acc n 0)
(quote "Eroare! argumentul nu e un numar")))

(define (sum-acc n acc)
  (if (= n 0) acc
      (sum-acc (quotient n 10) (+ acc (modulo n 10)))))

(sum 0)
(sum 1234)
(sum 'a)
