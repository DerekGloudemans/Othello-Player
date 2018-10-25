#lang racket
(require "strategies.rkt")
(require "utils.rkt")
(require "typed-othello-game-logic.rkt")

(define (while-list list)

  (if (empty? list)
      [(display "end")]
      [(display (first list))
       (while-list (rest list))]
      ))

