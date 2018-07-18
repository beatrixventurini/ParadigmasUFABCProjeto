#lang racket (require csv-reading)

(define next-row (make-csv-reader (open-input-file "teste.csv") ' ((separator-chars #,) (strip-leanding-whitespace? . #t) (strop-trailing-whitespace? . #t))))

(define (till-end-lines) ;seleciona uma linha e passa para o metodo passagem que trata o dado (define a (next-row)) ;pega um dado do csv

(if (null? a) ;se nulo retorna nulo e fim (println a) (passagem a)) ;else: aplicar metodo sobre o dado )

(define (passagem a) ;trata o dado e retorna para till-next-lines para buscar aprox linha

(print "aplicar um metodo aquipara ler o next-row = ") ; metodo para tratar o dado (println a)

(till-end-lines) ;retorna para buscar o prox dado )

