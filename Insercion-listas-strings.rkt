#lang scheme
#|
Funcion: insert
Descripcion: Ordena listas de strings e inserta elementos de strlst en list. 
Parametros:
strlst lista de strings
lst lista de strings
Retorno: lista ordenada formada por la union de strlst y list.
|#
(define (insert strlst lst)
  (let ((original (sort lst string<?)) (cola (sort strlst string<?)))
    (let ciclo ((queue cola) (mod original))
      (cond
        ((null? mod) (cond
                       ((= (length queue) 1) (cons (car queue) mod))
                       (else (cons (car queue) (ciclo (cdr queue) mod)))))
        ((string<? (car queue) (car mod))
         (cond
           ((= (length queue) 1) (cons (car queue) mod))
           (else (cons (car queue) (ciclo (cdr queue) mod)))))
        (else (cons (car mod) (ciclo queue (cdr mod)))))
    )
)
)
  
