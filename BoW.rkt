#lang racket
;Importação de pacotes
(require csv-reading)

;Importação e exportação de funções
(require "StopWords.rkt")
(provide treino_final Bow_00 Bow_11)

;;Gera a lista de presença de palavras das frases de teste.
;;Saída exemplo: '(0 0 0 1 0 0 1 0 0 1 0 0 0 0 0 1)
(define (BoWTest n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (force bow)))

(define (Bow_0 n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (car (force bow))))

(define (Bow_1 n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (car (cdr (force bow)))))

(define (Bow_00 n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (force BOW)))

(define (Bow_11 n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (force BOW)))


;;Gera a lista de treino, com as frases de treino convertidas em lista de 0's e 1's e tem como último elemento a categoria da classe
;;Saída exemplo: '((1 0 0 1 0 0 0 1 0 0 0 0 1 0) (0 1 0 0 0 1 0 1 0 0 0 0 1 1))
(define (treino x)
  (define next-row
    (leitor-csv (open-input-file x)))
  (next-row)
  (let dadosTreino ([treino_0 null] [data (next-row)] [treino_1 null])
    (cond  [(null? data) (cons treino_0 treino_1)]
           [(equal? (car (cdr data)) "0") (dadosTreino (cons (append (Bow_0 data) (list (string->number (car (cdr data))))) treino_0) (next-row) treino_1)]
           [else (dadosTreino treino_0 (next-row) (cons (append (Bow_1 data) (list (string->number (car (cdr data))))) treino_1))])))

(define (treino_final x)
  (define next-row
    (leitor-csv (open-input-file x)))
  (next-row)
  (let dadosTreino ([treino_0 null] [data (next-row)] [treino_1 null])
    (cond  [(null? data) (cons treino_0 treino_1)]
           [(equal? (car (cdr data)) "0") (dadosTreino (cons (append (Bow_00 data) (list (string->number (car (cdr data))))) treino_0) (next-row) treino_1)]
           [else (dadosTreino treino_0 (next-row) (cons (append (Bow_11 data) (list (string->number (car (cdr data))))) treino_1))])))

(define (contagem_0 n)
  (let loop ([return_0 null] [treino_0 (car n)])
    (cond [(null? (car treino_0)) (reverse return_0)]
          [else (loop (cons (foldl + 0 (map car treino_0)) return_0) (map cdr treino_0))])))

(define (contagem_1 n)
  (let loop ([return_1 null] [treino_1 (cdr n)])
    (cond [(null? (car treino_1)) (reverse return_1)]
          [else (loop (cons (foldl + 0 (map car treino_1)) return_1) (map cdr treino_1))])))

(define (BowFinal_0 n)
  (let loop ([bow_00 null] [lst (contagem_0 (treino "TreinoExemplo.csv"))] [bow (car (bow_aux "TreinoExemplo.csv"))])
    (cond [(null? bow) bow_00]
          [(> 2 (car lst)) loop (cons (car bow) bow_00) (cdr lst) (cdr bow)]
          [else (loop bow_00 (cdr lst) (cdr bow))])))

(define (BowFinal_1 n)
  (let loop ([bow_11 null] [lst (contagem_1 (treino "TreinoExemplo.csv"))] [bow (car (cdr (bow_aux "TreinoExemplo.csv")))])
    (cond [(null? bow) bow_11]
          [(> 2 (car lst)) loop (cons (car bow) bow_11) (cdr lst) (cdr bow)]
          [else (loop bow_11 (cdr lst) (cdr bow))])))

(define bowFinal_00 (delay (BowFinal_0 1)))

(define bowFinal_11 (delay (BowFinal_1 1)))

(define BOW (delay (flatten (cons (force bowFinal_00) (force bowFinal_11)))))

;;Gera o Bag of Words das frases da planilha de treino.
;;Saída exemplo: '("almoço" "familia" "escola" "futebol" "jogo" "bola")
(define (bow_aux x)
  (define next-row
    (leitor-csv (open-input-file x)))
  (next-row)
  (let test ([data  (next-row)] [Class_0 null] [Class_1 null])
    (cond [(null? data) (cons (remove-duplicates (flatten Class_0)) (list (remove-duplicates (flatten Class_1))))]
          [(equal? (car (cdr data)) "0") (test (next-row) (cons (removeStopwords data) Class_0) Class_1)]
          [else (test (next-row) Class_0 (cons (removeStopwords data) Class_1))])))

;;Executação preguiçosa da função bow_aux
;;Saída exemplo: '("almoço" "familia" "escola" "futebol" "jogo" "bola")
(define bow (delay (bow_aux "TreinoExemplo.csv")))

;;Verifica se uma palavra faz parte de uma lista, se fizer, retorna 1, se não, 0.
(define (ismember3? str strs) (if [member str strs] 1 0))

;Leitor CSV
(define leitor-csv
  (make-csv-reader-maker
   '((separator-chars            #\;))))

(define next-row
  (leitor-csv (open-input-file "TreinoExemplo.csv")))