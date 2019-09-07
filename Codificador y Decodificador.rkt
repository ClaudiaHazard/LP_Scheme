#lang scheme
#|
Funcion: codificador
Descripcion: Devuelve una lista con los caracteres cambiados dependiendo del codigo dado en la lista de asociacion.
Parametros:
    codigo: Lista de asociacion con los cambios de caracteres que hay que realizar.
    expr: Lista con la expresion original a ser cambiada.
Retorno: Lista con la expresion cambiada segun indica el codigo.
|#
(define (codificador codigo expr)
  (cond
    ((null? expr) #f)
    ((null? codigo) #f)

    ((equal? (length expr) 1)
       (cond
         ((equal? (assoc (car expr) codigo) #f) (list (car expr)))
         (else (list (cdr (assoc (car expr) codigo))))
       )
    )
    (else
       (cond
         ((equal? (assoc (car expr) codigo) #f) (cons (car expr) (codificador codigo (cdr expr))))
         (else (cons (cdr (assoc (car expr) codigo)) (codificador codigo (cdr expr))))
       )
     )
   )
)

#|
Funcion: reverse1
Descripcion: Funcion auxiliar que da vuelta la lista de asociacion del codigo.
Parametros:
   l: lista de asociacion con el codigo.
Retorno:  Lista de asociacion dada vuelta.
|#
(define (reverse1 l)
  (if (null? l)
     '()
     (cons (cons (cdar l) (caar l)) (reverse1 (cdr l)))
  )
)
#|
Funcion: decodificador
Descripcion: Devuelve una lista con los caracteres cambiados dependiendo del codigo dado en la lista de asociacion.
Parametros:
  codigo: lista de asociacion de caracteres.
  expr: lista de caracteres.
Retorno: Lista de caracteres decodificada. 
|#
(define (decodificador codigo expr)
  (let decode ((code (reverse1 codigo)) (ex expr))
    (cond
      ((null? ex) #f)
      ((null? code) #f)
      ((equal? (length ex) 1)
       (cond
         ((equal? (assoc (car ex) code) #f) (list (car ex)))
         (else (list (cdr (assoc (car ex) code))))
       )
      )
      (else
       (cond
         ((equal? (assoc (car ex) code) #f) (cons (car ex) (decode code (cdr ex))))
         (else (cons (cdr (assoc (car ex) code)) (decode code (cdr ex))))
       )
      )
    )
  )
)
