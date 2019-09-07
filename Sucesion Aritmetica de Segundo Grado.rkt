;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Sucesion Aritmetica de Segundo Grado|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
#|
Funcion: sucesioniter
Descripcion: Función que realiza una sucesión aritmetica determinada utilizando iteraciones.
Parametros:
  n: numero entero.
Retorno: n-esimo elemento de la sucesión.
|#
(define (sucesioniter n)
  (cond
    ((equal? n 0) 0)
    ((equal? n 1) 1)
    ((equal? n 2) 2)
    (else (+ (sucesioniter (- n 1)) (sucesioniter (- n 3))))))
#|
Funcion: sucesioncola
Descripcion: Función que realiza una sucesión aritmetica determinada utilizando recursividad de cola.
Parametros:
  n: entero.
Retorno: n-esimo elemento de la sucesión.
|#
(define (sucesioncola n)
  (cond
    ((equal? n 0) 0)
    ((equal? n 1) 1)
    ((equal? n 2) 2)
    (else
     (let ciclo ((nmenos3 0) (nmenos2 1) (nmenos1 2) (suma 2) (ene 3))
       (cond
         ((equal? ene n) suma)
         (else (ciclo nmenos2 nmenos1 suma (+ suma nmenos2) (+ 1 ene))))))))


                     
  