#lang racket
;Importação de pacotes
(require csv-reading)

;Importação e exportação de funções
(require "StopWords.rkt")
(provide treino Bow_0 Bow_1)

;;Gera a lista de presença de palavras das frases de teste.
;;Saída exemplo: '(0 0 0 1 0 0 1 0 0 1 0 0 0 0 0 1)
(define (BoWTest n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (force bow)))

(define (Bow_0 n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (car (force bow))))

(define (Bow_1 n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (car (cdr (force bow)))))

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