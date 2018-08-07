#lang racket

;função que separa um conjunto de dados de treino, de acordo com a porcentagem
;definida, p. ex. 0.67 (67%)

(define (splitDataset dataset splitRatio)
  (define trainSize (exact-floor (* (length dataset) splitRatio)))
  (let splitDataset-i ([trainSet '()])
    (cond [(> (length trainSet) trainSize) trainSet]
          [else (splitDataset-i (cons (list-ref dataset (random 0 trainSize)) trainSet))])))