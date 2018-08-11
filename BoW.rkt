#lang racket


(require csv-reading)

(define (BoWTest n)
  (map (lambda(x) (ismember3? x (removeStopwords n))) (bowAux "TreinoExemplo.csv")))

;(define (BowTrain n)
;  ()

(define (bowAux x)
  (define next-row
    (make-food-csv-reader (open-input-file x)))
  (next-row)
  (let test ([data  (next-row)] [aux null])
    (cond [(null? data) (remove-duplicates (flatten aux))]
          [else (test (next-row) (cons (removeStopwords data) aux))])))

(define (ismember3? str strs) (if [member str strs] 1 0))

(define make-food-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\;))))

(define next-row
  (make-food-csv-reader (open-input-file "TreinoExemplo.csv")))





(define stopwords '("de" "a" "o" "que" "e" "do" "da" "em" "um" "para" "é" "com" "não" "uma" "os" "no" "se" "na" "por" "mais" "as" "dos" "como" "mas" "foi" "ao" "ele" "das" "tem"
                          "à" "seu" "sua" "ou" "ser" "quando" "muito" "há" "nos" "já" "está" "eu" "também" "pelo" "pela" "até" "isso" "ela" "entre" "era" "depois" "sem"
                          "só" "mesmo" "aos" "ter" "seus" "quem" "nas" "me" "esse" "eles" "estão" "você" "tinha" "foram" "essa" "num" "nem" "suas" "meu" "às" "minha"
                          "têm" "numa" "pelos" "elas" "havia" "seja" "qual" "será" "nós" "tenho" "lhe" "deles" "essas" "esses" "pelas" "este" "fosse" "dele" "tu" "te"
                          "vocês" "vos" "lhes" "meus" "minhas" "teu" "tua" "teus" "tuas" "nosso" "nossa" "nossos" "nossas" "dela" "delas" "esta" "estes" "estas"
                          "aquele" "aquela" "aqueles" "aquelas" "isto" "aquilo" "estou" "está" "estamos" "estão" "estive" "esteve" "estivemos" "estiveram" "estava"
                          "estávamos" "estavam" "estivera" "estivéramos" "esteja" "estejamos" "estejam" "estivesse" "estivéssemos" "estivessem" "estiver" "estivermos" "estiverem" "hei" "há" "havemos" "hão" "houve" "houvemos" "houveram" "houvera" "houvéramos" "haja" "hajamos" "hajam" "houvesse" "houvéssemos" "houvessem" "houver" "houvermos" "houverem" "houverei" "houverá" "houveremos" "houverão" "houveria" "houveríamos" "houveriam" "sou" "somos" "são" "era" "éramos" "eram" "fui" "foi" "fomos" "foram" "fora" "fôramos" "seja" "sejamos" "sejam" "fosse" "fôssemos" "fossem" "for" "formos" "forem" "serei" "será" "seremos" "serão" "seria" "seríamos" "seriam" "tenho" "tem" "temos" "tém" "tinha" "tínhamos" "tinham" "tive" "teve" "tivemos" "tiveram" "tivera" "tivéramos" "tenha" "tenhamos" "tenham" "tivesse" "tivéssemos" "tivessem" "tiver" "tivermos" "tiverem" "terei" "terá" "teremos" "terão" "teria" "teríamos" "teriam"))

(define (removeAndSplit n)
  (let loop([lst (string-split (car n))] [lstFinal '()])
    (cond [(null? lst) lstFinal]
          [else (loop (cdr lst) (append lstFinal (string-split (car lst) #px"[.,;:!\\[\\-\\]\\?]")))])))

(define (removeStopwords lst)
  (let sw ([lstSW stopwords] [listaLimpa (removeAndSplit lst)])
    (cond
      [(null? lstSW) listaLimpa]
      [else (sw (cdr lstSW) (filter (lambda (x) (not (equal? (car lstSW) x))) listaLimpa))])))