#lang racket
(require racket/trace)

;;; Groza Anamaria
;;; Data: 20.03.2023
;;; Tema4: Familiarizare cu conceptul de iterativitate in Racket

;;;P#2 Definiti o functie care calculeaza inversa unei liste.
(define (rev-list l)
   (if (list? l)
   (do ((lista l (cdr lista))
        (solutie '() (cons (car lista) solutie)))
        ((null? lista) solutie)
     )
   "Argumentul nu este lista!"))
        

(rev-list '(a b c d e)) ; Rezultat : '(e d c b a)
(rev-list 24) ; Rezultat : "Argumentul nu este lista!"

;;;P#1 Definiti o functie pentru concatenarea a doua liste. Nu se va folosi append.


(define (concatenate-lists lst1 lst2)
  (define result '())
  (do ((i (- (length lst2) 1) (- i 1))) ((< i 0))
    (set! result (cons (list-ref lst2 i) result))) ;itereaza prin elementele listei 2 in ordine inversa si le adauga la rezultat 
  (do ((i (- (length lst1) 1) (- i 1))) ((< i 0))
    (set! result (cons (list-ref lst1 i) result ))) ;itereaza prin elementele listei 1 in ordine inversa si le adauga la rezultat 
  result)

(concatenate-lists '(5 4 3 2) '(1 2 3 4))

;;;P#3 Definiti o functie care extrage toate elementele impare dintr-o lista. Nu se va folosi predicatul odd?
(define (elem-odd ls)
  
  (define res '()) ; definim lista de rezultat
  (cond [(list? ls)
      (for ([i ls]) ; iteram prin lista primita ca argument
        (cond ((eq? (remainder i 2) 1) ;cand numarl e impar il adaugam la lista de rezultat
               (set! res (cons i res))
                                       )))
  (reverse res)]
  [else "Argumentul nu este lista!"]))

(elem-odd '(1 2 2 2 3 4 5 6))
(elem-odd 68)

;;;P#4 Definiti o functie care extrage elementele de pe pozitiile impare dintr-o lista.

(define (odds-positions ls)
  
  (define res '())
  (define k 0) ; definim un contor care ne va ajuta sa determinam pozitia
  (cond [(list? ls)
      (for ([i ls])
         (if (= 1 (remainder k 2)) ; cand contorul e impar actualizam lista de rezultat
              (set! res (cons i res)) ; adaugam valoarea de pe pozitia impara la lista de rezultat
              (set! res res))
          (set! k (+ 1 k)))
  (reverse res)]
  [else "Argumentul nu este lista!"]))

(odds-positions '(1 2 3 2 3 4 5))

;;;P#5 Definiti o functie pentru calculul iterativ al termenului de rang n, n >= 1, a sirului fibonacci ( f(n) = f(n-1) + f(n-2), pentru n >=3 , iar f(1) = f(2) = 1 )
(define (fibonacci n)
  ;initializam f(1), f(2), f(3) , adica a, b si c
  (define a 1)
  (define b 1)
  (define c 2)
  (cond
    ((not (number? n)) "Argumentul nu e număr!")
    ((< n 1) "Număr incorect!")
    ((= n 1) 1)
    ((= n 2) 1)
    ((= n 3) 2)
    (else
      (for ([i (- n 3)]) ; primii 3 termeni sunt deja fixati
        ;calculam valorile din sirul fibonacci
        (set! a b)
        (set! b c)
        (set! c (+ a b)))
      c)))

(fibonacci 5) ; 5
(fibonacci 6) ; 8
(fibonacci '(1 2)) ; "Argumentul nu e număr!"

;;;P#6 Definiti o functie pentru calculul iterativ al inversului unui numar.

;;; Am verificat daca argumentul primit este numar
;;; Daca este am calculat inversul dupa formula: invers = (ultima_cifra * 10^(numar_de_cifre - 1)) + inverse(rest_numar_de_cifre)
(define (inv n)
  (if (not (number? n)) ;verificam daca e numar
      "nu e numar"
      (let loop ([n n] [result 0] );initializam rezultatul cu 0
        (if (= n 0)
            result
            (loop (quotient n 10)
                  (+ (* result 10) (remainder n 10) ) ;calculam oglinditul dupa formula rezultat= rezultat*10 + n%10
                  )))))  


(inv 98765); 56789

;;;P#7 Folosindu-va de functia definita la punctul precedent, definiti predicatul (palindrome? x) care primeste un numar si returneaza #t daca acesta este palindrom si #f altfel.

(define (palindrome? x)
  (if (number? x) ; Am verificat daca argumentul primit este numar
       (if (= x (inv x)) #t #f) ;Am verificat daca numarul primit ca argument este egal cu inversul sau,atunci am returnat true si false in caz contrar
       (quote "Argumentul primit nu e numar")))

(palindrome? 6556)
(palindrome? "anna") ;"Argumentul primit nu e numar"