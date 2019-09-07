#lang scheme
(define suma 0)
#|
Funcion: integral
Descripcion: Funcion que resuelve una integral de un polinomio entre dos valores. 
Parametros:
  poly: lista con los coeficientes del polinomio.
  x: numero entero.
  y: numero entero.
Retorno: numero que resulta de la evaluacion de la integral.
|#
(define (integral poly)
  (lambda (x y)
    (let jon ((pol poly) (divexp (length poly)) (in x) (fn y))
    (cond
      ((null? pol) suma)

      
      (else  (set! suma (+ suma (/ (* (car pol) (expt in divexp)) divexp)))
              (set! suma (- suma (/ (* (car pol) (expt fn divexp)) divexp)))
              (jon (cdr pol) (- divexp 1) in fn))
      ))))




