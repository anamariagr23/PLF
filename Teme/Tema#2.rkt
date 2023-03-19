#lang racket
(require racket/trace)

;;; Groza Anamaria
;;; Data: 02.03.2023
;;; Tema2: Familiarizare cu conceptul de recursivitate in Racket.

;;;P#1 Definiti o functie pentru concatenarea a doua liste. Nu se va folosi append.

;;;> (conc ‘(1 2 3) ‘(4 5 6))
;;;‘(1 2 3 4 5 6)
;;;> (conc ‘(1 2 3) ‘())
;;;‘(1 2 3)
;;;> (conc ‘(1 2 3) ‘a)
;;;Eroare! argumentul 2 nu este o lista

;;;Verific daca argumentele sunt liste
;;;daca lista 1 e vida atunci returnez lista 2
;;;altfel iau capul listei 2 si apelez recursiv pentru coada listei 1 si lista 2
;;;lista 1 se va goli la un moment dat si va fi returnata lista 2
;;; la intoarcerea apelului recursiv se vor adauga la lista 2 elementele pe care le-am scos din lista 1
#|
>(conc '(1 2 3) '(4 5 6))
> (conc '(2 3) '(4 5 6))
> >(conc '(3) '(4 5 6))
> > (conc '() '(4 5 6))
< < '(4 5 6)
< <'(3 4 5 6)
< '(2 3 4 5 6)
<'(1 2 3 4 5 6)
|#

(define (conc list1 list2)
  (if (and (list? list1) (list? list2));;;Verific daca argumentele sunt liste
           (cond [(null? list1) list2];;;daca lista 1 e vida atunci returnez lista 2
                 [else (cons (car list1)(conc (cdr list1) list2))])
      (quote "Argumentele nu sunt liste")))
                 

(conc '(1 2 3) 'a)

;;;P#2 Definiti o functie care calculeaza inversa unei liste.
#|
> (rev ‘(1 2 3))
‘(3 2 1)
> (rev ‘a)
Eroare! Argumentul nu e o lista
> (rev ‘(a))
‘(a)
|#

;;;Verific daca argumentele sunt liste

(define (rev lst [result null])
  (if (list? lst)
      (cond [(null? lst) result]  ; base case
        [#t (rev (cdr lst)        ; recursive case
                     (cons (car lst) result))])
      (quote "Argumentul nu e lista")))

(trace rev)
(rev '(1 2 3 ))

;;;P#3 Definiti o functie care extrage toate elementele impare dintr-o lista. Nu se va folosi predicatul odd?. Se va parcurge lista in adancime.

;;; Pentru a parcurge lista in adancime am creat functia flatten ,
;;;care face o lista care contine elementele listei primite si ale sublistelor
;;;vom apela functia flatten in functia odds
;;;in functia odds am verificat daca capul listei e numar, daca este si este si impar l-am adaugat la noua lista
;;; de fapt numerele se vor adauga la revenirea din apelul recursiv


(define (flatten lst)
  (cond [(null? lst) null]
        [(pair? (car lst)) (conc (flatten (car lst)) (flatten (cdr lst)))]
        [else (cons (car lst) (flatten (cdr lst)))]))

(define (odds lst)
  (cond ((null? lst) '()) ; cazul de baza, lista e vida
        ((number? (car lst))
         (if (eq? (remainder (car lst) 2) 1)
             (cons (car lst) (odds (cdr lst)))
             (odds (cdr lst))))
         (else (odds (cdr lst)))) ; se ignora elementele care nu sunt numere
  )
                         

(trace odds)
(odds (flatten '(1 2 (1 (2 3 4 a)) (4 5 6))))

;;;P#4 Definiti o functie care extrage elementele de pe pozitiile impare dintr-o lista.

;;;> (odds-positions ‘(1 2 ‘(a b) 3 4))
;;;‘(1 ‘(a b) 4)

;;;Dacă lista este vidă, returnăm lista vidă.
;;;Dacă lungimea listei este impară, adăugăm primul element la lista rezultat și apelăm recursiv funcția pe restul listei.
;;;Altfel, apelăm recursiv funcția pe restul listei ignorând primul element.

(define (odds-positions lst)
  (cond [(null? lst) null]
        [(eq? (remainder (length lst) 2) 1) (cons (car lst) (odds-positions (cdr lst)))]
        [else (odds-positions (cdr lst))]))

(odds-positions '(1 2 (a b) 3 4))

;;;P#5 Definiti o functie pentru calculul recursiv al termenului de rang n, n >= 1, a sirului fibonacci ( f(n) = f(n-1) + f(n-2), pentru n >=3 , iar f(1) = f(2) = 1 )


(define (fib n)
  (if (number? n) ;Am verificat daca argumentul primit e numar
      (if (< n 2) n ; cazul de baza
          (+ (fib (- n 1)) (fib (- n 2)))
      )
   (quote "argumentul nu e numar"))
  )

(fib 6)

;;;P#6 Definiti o functie pentru calculul recursiv al inversului unui numar.
;> (inv 1234)
;4321
;> (inv ‘a)
;Eroare! argumentul nu e un numar


;;; Am verificat daca argumentul primit este numar
;;; Daca este am calculat inversul dupa formula: invers = (ultima_cifra * 10^(numar_de_cifre - 1)) + inverse(rest_numar_de_cifre)
(define (inv n)
  (if (number? n)
      (if (< n 2)
          n
          (+ (* (remainder n 10) (expt 10 (- (string-length (number->string n)) 1 )))(inv (quotient n 10))))
      (quote "nu e numar")))

        

(trace inv )
(inv 9873)

;;;P#7 Folosindu-va de functia definita la punctul precedent, definiti predicatul (palindrome? x) care primeste un numar si returneaza #t daca acesta este palindrom si #f altfel.


(define (palindrome? x)
  (if (number? x) ; Am verificat daca argumentul primit este numar
       (if (= x (inv x)) #t #f) ;Am verificat daca numarul primit ca argument este egal cu inversul sau,atunci am returnat true si false in caz contrar
       (quote "Argumentul primit nu e numar")))

(palindrome? "anna")
