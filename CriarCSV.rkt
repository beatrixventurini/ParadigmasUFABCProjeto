#lang racket
(require csv-reading)


;;;;;;;; IMPUT DA SAÍDA NO CSV ---- ADD NO FIM DO CÓDIGO   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rotulo = novo parametro a ser adicionado >> saídas do algoritmo
;metodo cria ou pega o arquivo nomeado, inputa os rotulos e fecha
(define (CriarCSV rotulo)
  (define openToWrite (open-output-file "rotulos.csv" #:exists 'append))
  (write rotulo openToWrite) ;neste caso vai até as aspas
  ;(display rotulo openToWrite) ;neste caso vai só o condeúdo dentro das ""
 (display ";" openToWrite) ;caso seja necessário acrescentar um separador
  (close-output-port openToWrite)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;metodo le o arquivo e transforma a linha uma string só
(define (CSVtoString)
 (define openToRead (open-input-file "rotulos.csv" ))
 (read-line openToRead))

;Le linha por linha o arquivo csv e converte em lista
(define reader (make-csv-reader-maker
                 '((separator-chars #\;)
                   (strip-leanding-whitespace? . #t)
                   (strop-trailing-whitespace? . #t))))

(define CSVtoList(reader (open-input-file "rotulos.csv")))




(define (LerCSV)
  (CSVtoList)
  )





;;;;;;;;;;;;;;;;;;;;;;;;;TESTE
(define (teste l)
(CriarCSV l)
  (if (> l 0) (teste (- l 1)) (LerCSV))
 )

(define (testedois l cont)
  (CriarCSV l)
  (if (> cont 0) (testedois l (- cont 1)) (LerCSV))
 )

