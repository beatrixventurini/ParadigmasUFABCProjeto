#lang racket

(require math/statistics)

(require "BoW.rkt")
(require "CriarCSV.rkt")

;;Separadores das frases de treino por classe
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

;Calcula a probabilidade
(define (calcProbab x media stdev)
  (cond [(= x 0) ( - 1 media)]
        [else media]))

;;Gera um resumo das probabilidades de cada um dos elementos(features) das frases de treino 
(define (summarize dataset)
  (let loop([lst dataset] [summary null])
  (cond [(null? (car (map cdr lst))) (reverse summary)]
        [else (loop (map cdr lst) (cons (list (mean (map car lst)) (stddev (map car lst))) summary))])))

;;Resumo das classes distintas
(define summarize_0 (delay (summarize (car (treino_final "TreinoExemplo.csv")))))
(define summarize_1 (delay (summarize (cdr (treino_final "TreinoExemplo.csv")))))

;;Recebe uma frase como entrada e calcula a probabilidade dela ser de uma classe ou de outra
(define (calculateClassProb frase dataset)
  (let loop ([prob1 0.5] [prob2 0.5] [frase_0 (Bow_00 frase)] [frase_1 (Bow_11 frase)] [class_0 (force summarize_0)] [class_1 (force summarize_1)])
    (cond
      [(not (null? frase_0)) (loop (* prob1 (calcProbab (car frase_0) (caar class_0) (cadar class_0))) prob2 (cdr frase_0) frase_1 (cdr class_0) class_1)]
      [(not (null? frase_1)) (loop prob1 (* prob2 (calcProbab (car frase_1) (caar class_1) (cadar class_1))) frase_0 (cdr frase_1) class_0 (cdr class_1))]
      [else (cons prob1 prob2)])))

;;Realiza a predição
(define (predict frase)
  (let prob_20 ([valorClasses (calculateClassProb frase "TreinoExemplo.csv")])
    (cond [(< (car valorClasses) (cdr valorClasses)) "familia"]
          [else "trabalho"])))


;;;;;;;;;;;; FUNÇÃO CHAVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (classificar lista) ;A lista deve terminar com () no ultimo valor
  (define (acumulador a lista)
    (cond [(null? (cdr a)) (SendToCSV (reverse lista))] ;envia a lista para criar o arquivo csv
          ;arma122ena os valores do predict na lista e lê o pro120imo ane120o
          [else (acumulador (cdr a) (cons (predict (car a)) lista))]))
  
(acumulador lista null))
