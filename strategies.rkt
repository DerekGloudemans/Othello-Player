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

;-----------------------------BEGIN STUDENT CODE-------------------------------;

; minimax - selects the best move for player on board after searching depth 
; levels, using eval-fn to score the board resulting from each move
; Returns two values - a value (for the selected move) and a move
(define (minimax player board depth eval-fn)

  ; Initialize return values with default values; out-val will always 
  ; be updated at least once prior to being returned
  (define out-move #f)
  (define out-val -99999)
  
  ; create list of possible moves for player and opponent
  (define moves (legal-moves player board))
  (define opponent-moves (legal-moves (opponent player) board))

  ; define sub-function for iterating through list of possible moves
  (define (list-iter list)
     (cond
       ; Case 1: if there are no remaining available moves, do nothing
       [(empty? list) (void)]
       ; Case 2: there is at least one remaining move to be assessed
       [else
          ; call minimax to return value for first remaining move in list 
          (let-values ([(val move)
                        (minimax (opponent player)
                                 (make-move (first list) player board)
                                 (- depth 1) eval-fn)])

            ; update return values if - val > out-val (i.e. new best move)
            (set! val (- 0 val))
            (cond
              [(> val out-val)
                (set! out-val val)
                (set! out-move (first list))   
              ]
            )
          ); end let

          ; call list-iter on rest of list of available moves
          (list-iter (rest list))
       ]
     )
  ); end list-iter definition

  
  ; deal with possible game cases for an iteration of minimax
  (cond

    ; Case 1: depth is 0
    ; set out-val to board value
    [(equal? 0 depth) (set! out-val (eval-fn player board))]
    
    ; Case 2: Neither player can move
    ; set out-val final board value
    [(and (empty? moves) (empty? opponent-moves))
     (set! out-val (final-value player board))]

    ; Case 3: Opponent only can move
    ; call minimax for opponent, throw away move, set out-val to - returned val
    [(empty? moves)
     (let-values ([(val move)
                   (minimax (opponent player) board depth eval-fn)])
          (set! out-val (- 0 val))  
     )]
    
    ; Case 4: Current player can move
    ; cycle through list of available moves using list-iter
    [else
         (list-iter moves)
    ]
  
  );end cond
  
  ; return out-move and out-val
  (values out-val out-move)
  
); end of minimax definition


(define (minimax-searcher depth eval-fn)
  "A strategy that searches DEPTH levels and then uses EVAL-FN."
  (let ([output (lambda (player board)
                  (let-values ([(value move)
                                (minimax player board depth eval-fn)])
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


; alpha-beta - selects the best move for player on board after searching
; depth levels, using eval-fn to score the board resulting from each move
; nodes are pruned to increase computational efficiency using alpha-beta
; pruning- achievable value passed from higher to lower levels as achievable
; due to implementation, cutoff is unused
; Returns two values - a value (for the selected move) and a move
(define (alpha-beta player board achievable cutoff depth eval-fn)  

  ; Initialize return values with default values; out-val will always 
  ; be updated at least once prior to being returned
  (define out-move #f)
  (define out-val -99999)
  
  ; create list of possible moves for player and opponent
  (define moves (legal-moves player board))
  (define opponent-moves (legal-moves (opponent player) board))

    ; define sub-function for iterating through list of possible moves
  (define (list-iter list)
     (cond
       ; Case 1: list of remaining possible moves is empty
       [(empty? list) (void)]
       ; Case 2: list of remaining possible moves is not empty
       [else
          ; recursively call alpha-beta on first list element
          ; out-val is known to be possible, so if subsequent layer finds
          ; a node higher than out-val (i.e. worse for this layer
          ; than already achievable) prune rest of this node
          (let-values ([(val move)
                        (alpha-beta (opponent player)
                                    (make-move (first list) player board)
                                    out-val 0 (- depth 1) eval-fn)])
            ; update out-val and out-move if - val > out-val
            (set! val (- 0 val))
            (cond
              [(> val out-val)
                (set! out-val val)
                (set! out-move (first list))   
              ]
            )
            
            ; if, after this update, - out-val is less than achievable,
            ; prune rest of node
            ;(it will be returned and disregarded by higher level)
            (cond
              [(> (- 0 out-val) achievable)
               ; call list-iter on rest list
               (list-iter (rest list))]
            )
          ); end let-values
       ] 
     )
  ); end list-iter definition
  
   ; deal with possible game cases for an iteration of minimax
  (cond

    ; Case 1: Depth is 0
    ; set out-val to board value
    [(equal? 0 depth) (set! out-val (eval-fn player board))]
    
    ; Case 2: Neither player can move
    ; set out-val final board value
    [(and (empty? moves) (empty? opponent-moves))
     (set! out-val (final-value player board))]

    ; Case 3: Opponent only can move
    ; call minimax for opponent, throw away move, set out-val to - returned val
    [(empty? moves)
     (let-values ([(val move) (minimax (opponent player) board depth eval-fn)])
          (set! out-val (- 0 val))  
     )]
    
    ; Case 4: Current player can move
    ; cycle through list of available moves using list-iter
    [else
         (list-iter moves)
    ]
  
  );end cond
  
  ; return out-move and out-val
  (values out-val out-move)
  
); end of alpha-beta definition


;------------------------------ END STUDENT CODE -------------------------------;

(define (alpha-beta-searcher depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  (let ([output (lambda (player board)
                  (let-values ([(value move)
                                (alpha-beta player board
                                            losing-value
                                            winning-value
                                            depth
                                            eval-fn)])
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