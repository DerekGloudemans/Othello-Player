#lang racket
(require "strategies.rkt")
(require "utils.rkt")
(require "typed-othello-game-logic.rkt")

(define (while-list list m)

  (cond  
      [(empty? list) m ]
      [(> (first list) m) (while-list (rest list) (first list))]
      [else (while-list (rest list) m)]
      ))

(define x (list 1 2 3 4 54 5 6 7 778 5 4 32 21))


(define m -10000)
(> (first x) m)
m
(set! m (first x))
m
(display (while-list x 0))