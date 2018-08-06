#lang racket

(define (splitDataset dataset splitRatio)
  (let ([trainSize (exact-floor (* (length dataset) splitRatio))]
        [trainSet '()])
  (cond [(> (length trainSet) trainSize) trainSet]
        [else (begin(cons (list-ref dataset (random 0 trainSize)) trainSet)
                    (splitDataset dataset splitRatio))])))

;não está ok ainda
