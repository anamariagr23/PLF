#lang racket


; 1. Definiti o functie recursiva care calculeaza suma elementelor unei liste, varianta iterativa

(define (sum list)
  (if (not (list? list))
      "Eroare! Argumentul nu este lista!"
      (let ((s 0))
        (for ([i list])
          (if (number? i)
              (set! s (+ s i))
              s
          )
        )
        s
     )
   )
)

;(sum '(1 a 2 3))


; 2. Definiti o functie recursiva care, iterativ, calculeaza suma S = 1 + 1/2 + 1/3 + ... + 1/n, pentru n numar dat

(define (sum2 n)
  (if (not (number? n))
      "Eroare! Argumentul nu este numar!"
      (let ((s 0))
        (for ([i (+ n 1)])
          (if (> i 0)
              (set! s (+ s (/ 1 i)))
              s
          )
        )
        s
      )
  )
)

;(sum2 2)


; 3. Definiti o functie recursiva care, pe baza unui numar natural n, construieste lista cu diviziorii acestuia, iterativ

(define (div n)
  (if (not (number? n))
      "Eroare! Argumentul nu este numar!"
      (let ((l '()))
        (for ([i (in-range 1 (+ n 1))])
          (if (eq? (remainder n i) 0)
              (set! l (cons i l))(void)))
        (if (null? l)
            "Numarul nu are divizori."
            (reverse l))
      )
   )
)





;(div 'a)
;(div 6)
;(div 12)



; 4. Definiti o functie care calculeaza media aritmetica a n numere reprezentate printr-o lista. Functia va primi lista ca si parametru si va returna media aritmetica a numerelor prezente in lista, iterativ




(define (ma l)
  (if (not (list? l))
      "Eroare! Argumentul nu este lista!"
      (let ((s 0) (nr 0))
        (for ([i l])
          (if (number? i)
              (begin
                (set! s (+ s i))
                (set! nr (+ nr 1)))
              (void)
              ))
        (if (= nr 0)
            "Lista nu contine numere."
            (/ s nr)))))

(ma '(0 1))
(ma '(1 2 3))
(ma '(1 2 3 3 5))