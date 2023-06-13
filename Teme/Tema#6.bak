#lang racket
 (require racket/trace)

;creare nr complexe
(define(make-complex a b)
  (cons a b))

;definim cateva nr complexe
(define c1 (make-complex -1 5))
(define c2 (make-complex 2 -4))
(define c3 (make-complex 2 -3))
(define c4 (make-complex 6 1))
(define c5 (make-complex 0 -1))

;;;adunare
(define (add c1 c2)
  (cons (+ (car c1) (car c2)) (+ (cdr c1) (cdr c2))))

;; functie primeste un nr variabil de argumente, numere complexe și calculează suma lor
(define (add_var . args)
  (let loop ([args args] [sum (make-complex 0 0)])
    (if (null? args)
        sum
        (loop (cdr args) (add sum (car args)))))) 

(add c1 c2)

;;;scaderea
(define (scad c1 c2)
  (cons (- (car c1) (car c2)) (- (cdr c1) (cdr c2))))

;; functie primeste un nr variabil de argumente, numere complexe și calculează diferenta lor
(define (scad_var . arg)
    (let ([dif (car arg)])
      (for ([i (cdr arg)])
        (set! dif (scad dif i)))
      dif))
(scad c1 c2)


;;;inmultirea
(define (inm c1 c2)
  (cons (- (* (car c1) (car c2)) (* (cdr c1) (cdr c2))) (+ (* (car c1) (cdr c2)) (* (car c2) (cdr c1)))))

;; functie primeste un nr variabil de argumente, numere complexe și calculează produsul lor
(define (inm_var . arg)
    (let ([prod (car arg)])
      (for ([i (cdr arg)])
        (set! prod (inm prod i)))
      prod))

(inm c1 c2)

;;;impartirea
(define (imp c1 c2)
  (if (and (eq? (car c2) 0) (eq? (cdr c2) 0)) "Eroare! Numitorul nu poate fi 0"
  (cons (/ (+ (* (car c1) (car c2)) (* (cdr c1) (cdr c2))) (+ (expt (car c2) 2) (expt (cdr c2) 2))) (/ (- (* (car c2) (cdr c1)) (* (car c1) (cdr c2))) (+ (expt (car c2) 2) (expt (cdr c2) 2))))))

(imp c1 c2)
(imp c3 c4)

;; functie primeste un nr variabil de argumente, numere complexe și calculează raportul lor
(define (imp_var . arg)
    (let ([raport (car arg)])
      (for ([i (cdr arg)])
        (set! raport (imp raport i)))
      raport))

;;;modulul
(define (modul c)
  (sqrt (+ (expt (car c) 2) (expt (cdr c) 2))))

(modul c1)
(modul c2)

;;reprezentarea
(define (repr c)
  (if (= (car c) 0)
      (if (= (cdr c) 0) "0"
          (string-append (string-append "" (format "~v"(cdr c))) "i"))
  (if (= (cdr c) 0) (string-append "" (format "~v"(car c)))
  (if (< (cdr c) 0) (string-append (string-append (string-append "" (format "~v"(car c))) (format "~v"(cdr c))) "i")
      (string-append (string-append (string-append (string-append "" (format "~v" (car c))) "+") (format "~v"(cdr c))) "i")))))

(repr c1)
(repr c2)
(repr c3)
(repr c4)
(repr c5)

;definesc liste de nr complexe
(define lista (list c1 c2))
(define lista_1 (list c1 c2 c3 c4))
lista

;verific daca modulul primului nr e mai mic decat celui de al 2-lea
(define (module< n1 n2)
  (< (modul n1) (modul n2)))

(module< c1 c2)

;sorteaza lista crescator dupa modul
(sort lista module<)

;printez primul elem al listei sortate
(car (sort lista module<))

;sortare descrescatoare dupa modul
(car (sort lista (lambda (n1 n2)
            (> (modul n1) (modul n2)))))

;filtreaza nr din lista_1 care au partea reala intre 3 si 5
(map (lambda (number)
         (when (and (>= (car number) 3) (<= (car number) 5)) number
             )) lista_1)
;(trace add_var)
(apply add_var lista_1)
(apply scad_var lista_1)
(apply inm_var lista_1)
(apply imp_var lista_1)




