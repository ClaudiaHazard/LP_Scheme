#lang scheme
#|
Funcion: iteracion
Descripcion: Realiza una funcion al resultado del valor obtenido anterior realizando el proceso n veces. 
Parametros:
  func: funcion cualquiera.
  value: valor al cual aplicar la funcion.
  n: numero.
Retorno: Resultado de realizar la operación n veces en un valor.
|#
(define (iteracion func value n)
  (let john ((x func) (y value) (z n) (num 1))
    (cond
      ((equal? num 1) (john x (x) n 2))
      (else
       (cond
         ((equal? z 0) y)
         (else (john x (x y value) (- z 1) 2) )
       )
      )
    )
  )
)
