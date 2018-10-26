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
  ; This function must output a board value and a move
  ; Instantiate return values with default values
  ; Note: standard expression #<void> used to do nothing
  
  (define out-move #f)
  (define out-val -10000)

  ; create list of possible moves for player and opponent
  (define moves (legal-moves player board))
  (define opponent-moves (legal-moves (opponent player) board))

  ; define function for iterating through lists
  (define (list-iter list)
     (cond
       [(empty? list) (void)]
       [else
          ; call minimax on first list element
          (let-values ([(val move) (minimax (opponent player) (make-move (first list) player board) (- depth 1) eval-fn)])
            ; update out-val and out-move if -val (since it's the opponent's best move) > out-val
            (if (> (- 0 val) out-val)
                (
                 (set! out-val val)
                 (set! out-move (first list))
                 )
                (void)
            )
            

          ); end let

          ; call list-iter on rest list
          (list-iter (rest list))
       ]
     )
  )
  
  ; deal with all cases by testing entry conditions in cond statement
  (cond

    ; Case 1: Depth is 0
    ; set out-val to board value
    [(equal? 0 depth) (set! out-val (eval-fn player board))]
    
    ; Case 2: Neither player can move
    ; set out-val final board value
    [(and (empty? moves) (empty? opponent-moves)) (set! out-val (final-value player board))]

    ; Case 3: Opponent only can move
    ; call minimax for opponent, throw away move, set out-val to - returned val
    [(and (empty? moves) (not (empty? opponent-moves)))
     (let-values ([(val move) (minimax (opponent player) board depth eval-fn)])
          (set! out-val (- 0 val))  
     )
    ]
    
    ; Case 4: Current player can move
    ; cycle through list of available moves
    ; check if list is not empty
    ; call minimax depth -1 on each
    ; update move and val each time a new maximum move is achieved
    [else
         (cond
           [(empty? moves)]
         )
    ]; end Case 4 else

    
  );end cond
  
  ; return out-move and out-val
  (values out-val out-move)


  

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