#lang racket

(require math/statistics)

;Assumindo que o último elemento das "sub-listas" indica a qual classe ela pertence (0 ou 1)
;Funções abaixo fazem a separação

(define (separateByClass_0 dataset)
  (let loop([lst dataset] [separated_0 '()])
  (cond [(null? lst) separated_0]
        [(eq? (last (car lst)) 0) (loop (cdr lst) (cons (car lst) separated_0))]
        [else (loop (cdr lst) separated_0)])))

(define (separateByClass_1 dataset)
  (let loop([lst dataset] [separated_1 '()])
  (cond [(null? lst) separated_1]
        [(eq? (last (car lst)) 1) (loop (cdr lst) (cons (car lst) separated_1))]
        [else (loop (cdr lst) separated_1)])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Testes:
;(define x (list '("água" "suco" "refri" 0) '("cerveja" "vodka" "vinho" 1) '("whisky" "licor" "rum" 1)))
;(separateByClass_0 x)
;(separateByClass_1 x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(mean '(1 2 3 4 5))
;(stddev '(1 2 3 4 5))

(define (calcProbab x media stdev)
  (* (/ 1 (* (sqrt (* 2 pi)) stdev))
     (exp (/ (* -1 (expt (- x media) 2)) (* 2 (expt stdev 2))))))
;;;;;;;;;Teste;;;;;;;;;;;;;;;;;;;
;x= 71.5, média=73. desvio padrao = 6.2, probabilidade deve ser aprox. 0.062489
(calcProbab 71.5 73 6.2)
 
