#lang racket
;; so we can randomly choose a move
(require racket/random)

(require "typed-othello-game-logic.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define (random-strategy player board)
  "A strategy that randomly chooses a move."
  (random-ref (legal-moves player board)))

(define (maximize-difference player board)
  "A strategy that maximizes the difference in pieces."
  ((maximizer count-difference) player board))

(define (maximize-weight player board)
  "A strategy that maximizes the weighted difference in pieces."
  ((maximizer weighted-squares) player board))

(define (minimax player board depth eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching DEPTH levels deep and backing up values."
  ;; YOUR CODE HERE
  (values (eval-fn player board) (random-strategy player board)))

(define (minimax-searcher depth eval-fn)
  "A strategy that searches DEPTH levels and then uses EVAL-FN."
  (let ([output (lambda (player board)
                  (let-values ([(value move) (minimax player board depth eval-fn)])
                    move))])
    (set! output (procedure-rename output
                                   (string->symbol
                                    (string-append
                                     "minimax-searcher-"
                                     (number->string depth)
                                     "-"
                                     (symbol->string
                                      (object-name eval-fn))))))
    output))

(define (alpha-beta player board achievable cutoff depth eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching DEPTH levels deep and backing up values,
  using cutoffs whenever possible."
  ;; YOUR CODE HERE
  (values (eval-fn player board) (random-strategy player board)))

(define (alpha-beta-searcher depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  (let ([output (lambda (player board)
                  (let-values ([(value move) (alpha-beta player board losing-value winning-value depth eval-fn)])
                    move))])
    (set! output (procedure-rename output
                                   (string->symbol
                                    (string-append
                                     "alpha-beta-searcher-"
                                     (number->string depth)
                                     "-"
                                     (symbol->string
                                      (object-name eval-fn))))))
    output))