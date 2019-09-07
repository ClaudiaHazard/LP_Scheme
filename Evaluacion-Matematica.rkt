#lang scheme
#|
Funcion: mateval
Descripcion: Realiza expresiones matemáticas.
Parametros:
   expr: lista
Retorno: Resultado de la expresion matemática ingresada.
|#
(define (mateval expr)
    (cond
        ((list? expr)
           (cond
               ((equal? 'ADD (car expr)) (+ (mateval (cadr expr)) (mateval (caddr expr))))
               ((equal? 'SUB (car expr)) (- (mateval (cadr expr)) (mateval (caddr expr))))
               ((equal? 'MUL (car expr)) (* (mateval (cadr expr)) (mateval (caddr expr))))
               ((equal? 'DIV (car expr)) (/ (mateval (cadr expr)) (mateval (caddr expr))))
               (else (modulo (mateval (cadr expr)) (mateval (caddr expr))))
            )
         )

        (else expr)
      )
)
