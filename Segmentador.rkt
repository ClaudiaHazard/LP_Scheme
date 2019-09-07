;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Segmentador) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
Funcion: iguales
Descripcion: Función que separa los valores iguales continuos en una lista.
Parametros:
 lista: lista de numeros
Retorno: lista de valores iguales continuos y resto de la lista.
|#
(define (iguales lista)
  (cond
    ((null? (cdr lista))(list (car lista)))
    ((= (car lista) (car(cdr lista)))(cons (car lista) (iguales (cdr lista))))
    (else (list (car lista)(cdr lista))))
  )
#|
Funcion: pares
Descripcion: Función que separa los valores pares continuos en una lista.
Parametros:
 lista: lista de numeros
Retorno: lista de valores pares continuos y resto de la lista.
|#
(define (pares lista)
  (cond
    ((null? (cdr lista))(list (car lista)))
    ((= (car lista) (car(cdr lista)))(cons (cdr (reverse (iguales lista))) (pares (car (reverse (iguales lista))))))
    ((integer? (/ (car lista) 2))(cons (car lista) (pares (cdr lista))))
    (else (list (cons (car lista) (cdr lista))))
    )
  )
#|
Funcion: impares
Descripcion: Función que separa los valores impares continuos en una lista.
Parametros:
 lista: lista de numeros
Retorno: lista de valores impares continuos y resto de la lista.
|#
(define (impares lista)
  (cond
    ((null? (cdr lista))(list (car lista)))
    ((= (car lista) (car(cdr lista)))(cons (cdr (reverse (iguales lista))) (impares (car (reverse (iguales lista))))))
    ((not (integer? (/ (car lista) 2)))(cons (car lista) (impares (cdr lista))))
    (else (list (cons (car lista) (cdr lista))))
    )
  )
#|
Funcion: segmentador
Descripcion: Función que segmenta los valores pares impares e iguales continuos en una lista.
Parametros:
 lista: lista de numeros
Retorno: lista segmentada de numeros.
|#
(define (segmentador lista)
  (cond
  ((null? lista) (list ))
  ((and (null? (cdr lista)) (not (null? (car lista))))(list (car lista)))
  ((and (integer? (/ (car lista) 2)) (integer? (/ (car(cdr lista)) 2)))(cons (reverse (cdr (reverse (pares lista)))) (segmentador (car (reverse (pares lista))))))
  ((and (not (integer? (/ (car lista) 2))) (not (integer? (/ (car(cdr lista)) 2))))(cons (reverse (cdr (reverse (impares lista)))) (segmentador (car (reverse (impares lista))))))
  ((and (not (integer? (/ (car lista) 2))) (integer? (/ (car(cdr lista)) 2)))(cons (car lista) (segmentador (cdr lista))))
  ((and (integer? (/ (car lista) 2)) (not (integer? (/ (car(cdr lista)) 2))))(cons (car lista) (segmentador (cdr lista))))
  (else (list ))
  )
  ) 

(segmentador '(1 2 4 4 6 7 5 2 3))