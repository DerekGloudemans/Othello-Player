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

  ; get a list of all available moves
  (define moves (legal-moves player board))

  (cond
  [(equal? 0 depth) (values (eval-fn board) board)]
  ; if neither player can move, return final value for board (an integer)
  [(and  (empty? moves) (empty? (legal-moves (opponent player) board))) (values (final-value player board) board) ]
  ; if only opponent has moves, return #f and negative max score for opponent
  [(and  (empty? moves) (not (empty? (legal-moves (opponent player) board))) (values (minimax (opponent player) board depth eval-fn) #f))]
  ; both players have valid moves
  [else
    (define (while list depth maxval maxboard)

      ; stores value of first board on list
      (define current-board-val (- 0 (minimax (opponent player) (first list) (- depth 1) eval-fn)))

      (cond
        
        ; returns max scoring board once all boards have been checked
        [(empty? list) maxboard ]
        ; check if value of first board on list is greater than maxval and replace if so, then do rest of list
        [(> current-board-val maxval)
         (set! maxval current-board-val)
         (set! maxboard (first list))
         (while (rest list) depth maxval maxboard)
         ]
        ; do rest of list
        [else (while (rest list) depth maxval maxboard)]
        
           ) ; end of cond definition 
      
        ); end of while definition
    
      ] ; end of else block

)
  
  ; else continue with body of function

  ) ; end of cond

); end of function 






































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