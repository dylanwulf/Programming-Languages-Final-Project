(define eval-count 0)
(define cut-count 0)
(define total-branches 0)
(define times-branched 0)

;displays end-of-game statistics
(define (display-statistics)
  (display "1. Number of evaluations: ")(display eval-count)
  (display "2. Number of cuts: ")(display cut-count)
  (display "3. Average branch factor: ")(display avg-branch-factor))

;Returns average branching factor
(define (avg-branch-factor)
  (/ total-branches times-branched))

;True if the specified list contains the element x, or false otherwise
(define (contains list x)
    (cond ((null? list) #f)
        ((equal? (car list) x) #t)
        (else (contains (cdr list) x))))

;Returns 100 if the board is a win for the player, 0 if the board is a loss for the player, and otherwise, returns
;the ratio of player moveable pieces to opponent moveable pieces
;board: the state of the board
;whose-turn: 'x or 'o, depending on whose turn it is
;us: whichever of 'x or 'o that the computer player is
;opp: whichever of 'x or 'o that the opponent is
(define (evaluate board whose-turn us opp)
  (set! eval-count (+ eval-count 1))
  (cond
    ((= (number-moveable board opp 0 0) 0)
     100)
    ((= (number-moveable board us 0 0) 0)
     0)
    (else (/ (number-moveable board us 0 0)
     (number-moveable board opp 0 0)))))

(define (leaf board whose-turn us opp)
  (or
   (and (= (number-moveable board opp 0 0) 0) (eqv? whose-turn opp))
   (and (= (number-moveable board us 0 0) 0) (eqv? whose-turn us))
  )
)

;Returns the sum of the moveable pieces for a player in each row
;board: the state of the board
;whose-turn: 'x or 'o, depending on whose turn it is
;rowIndex: the row being checked
;total: the current sum of the moveable pieces (should be initially called as 0)
(define (number-moveable board whose-turn rowIndex total)
  (cond ((= rowIndex 8) total)
        (else (number-moveable board whose-turn (+ rowIndex 1) (+ total (number-moveable-row board whose-turn rowIndex 0 0))))))

;Returns the number of moveable pieces for a player in a row
;board: the state of the board
;whose-turn: 'x or 'o, depending on whose turn it is
;rowIndex: the row being checked
;colIndex: the column being checked
;total: the current sum of the moveable pieces (should be initially called as 0)
(define (number-moveable-row board whose-turn rowIndex colIndex total)
  (cond
    ((> colIndex 7) total)
    ((eqv? whose-turn (vector-ref (vector-ref board rowIndex) colIndex))
     (cond
       ((and
         (< (+ rowIndex 2) 8)
         (eqv? '- (vector-ref (vector-ref board (+ rowIndex 2)) colIndex))
         (not (eqv? '- (vector-ref (vector-ref board (+ rowIndex 1)) colIndex))))
        (number-moveable-row board whose-turn rowIndex (+ colIndex 2) (+ total 1)))
       ((and
         (> (- rowIndex 2) -1)
         (eqv? '- (vector-ref (vector-ref board (- rowIndex 2)) colIndex))
         (not (eqv? '- (vector-ref (vector-ref board (- rowIndex 1)) colIndex))))
        (number-moveable-row board whose-turn rowIndex (+ colIndex 2) (+ total 1)))
       ((and
         (< (+ colIndex 2) 8)
         (eqv? '- (vector-ref (vector-ref board rowIndex) (+ colIndex 2)))
         (not (eqv? '- (vector-ref (vector-ref board rowIndex) (+ colIndex 1)))))
        (number-moveable-row board whose-turn rowIndex (+ colIndex 2) (+ total 1)))
       ((and
         (> (- colIndex 2) -1)
         (eqv? '- (vector-ref (vector-ref board rowIndex) (- colIndex 2)))
         (not (eqv? '- (vector-ref (vector-ref board rowIndex) (- colIndex 1)))))
        (number-moveable-row board whose-turn rowIndex (+ colIndex 2) (+ total 1)))
       (else (number-moveable-row board whose-turn rowIndex (+ colIndex 2) total))))
    (else (number-moveable-row board whose-turn rowIndex (+ colIndex 1) total))))

;Makes a copy of the board so that applying hypothetical moves to it
;won't affect the real board
(define (board-copy board)
  (list->vector (map list->vector (map vector->list (vector->list board))))
)

;Returns a list of all possible moves that can be made by the current player
(define (possible-moves board whose-turn rowIndex movesList)
  (cond ((= rowIndex 8) movesList)
        (else (possible-moves board whose-turn (+ rowIndex 1) (append movesList (possible-moves-row board whose-turn rowIndex 0 '()))))))

;Returns a list of all possible moves that can be made by the current player in a row
(define (possible-moves-row board whose-turn rowIndex colIndex movesList)
  (cond
    ((> colIndex 7) movesList)
    ((eqv? whose-turn (vector-ref (vector-ref board rowIndex) colIndex))
       (possible-moves-row board whose-turn rowIndex (+ colIndex 2)
                           (append-move-up board colIndex rowIndex colIndex rowIndex
                                              (append-move-down board colIndex rowIndex colIndex rowIndex
                                                                (append-move-right board colIndex rowIndex colIndex rowIndex
                                                                                (append-move-left board colIndex rowIndex colIndex rowIndex movesList)))))
     )
    (else (possible-moves-row board whose-turn rowIndex (+ colIndex 1) movesList))))

;Attempts to append a move to the right for the specified piece.
;If it is possible, appends the move.
;If it is not possible, returns the moves list unaltered.
(define append-move-up
  (lambda (board colIndex rowIndex origCol origRow movesList)
    (if
      (and
        (< (+ rowIndex 2) 8)
        (eqv? '- (vector-ref (vector-ref board (+ rowIndex 2)) colIndex))
        (not (eqv? '- (vector-ref (vector-ref board (+ rowIndex 1)) colIndex)))
      )
      (append-move-up board colIndex (+ 2 rowIndex) origCol origRow (append movesList (list (list (list origCol origRow) (list colIndex (+ rowIndex 2))))))
      movesList
    )
  )
)

;Attempts to append a move to the left for the specified piece.
;If it is possible, appends the move.
;If it is not possible, returns the moves list unaltered.
(define append-move-down
  (lambda (board colIndex rowIndex origCol origRow movesList)
    (if
      (and
        (> (- rowIndex 2) -1)
        (eqv? '- (vector-ref (vector-ref board (- rowIndex 2)) colIndex))
        (not (eqv? '- (vector-ref (vector-ref board (- rowIndex 1)) colIndex)))
      )
      (append-move-down board colIndex (- rowIndex 2) origCol origRow (append movesList (list (list (list origCol origRow) (list colIndex (- rowIndex 2))))))
      movesList
    )
  )
)

;Attempts to append a move upwards for the specified piece.
;If it is possible, appends the move.
;If it is not possible, returns the moves list unaltered.
(define append-move-right
  (lambda (board colIndex rowIndex origCol origRow movesList)
    (if
      (and
        (< (+ colIndex 2) 8)
        (eqv? '- (vector-ref (vector-ref board rowIndex) (+ colIndex 2)))
        (not (eqv? '- (vector-ref (vector-ref board rowIndex) (+ colIndex 1))))
      )
      (append-move-right board (+ colIndex 2) rowIndex origCol origRow (append movesList (list (list (list origCol origRow) (list (+ colIndex 2) rowIndex)))))
      movesList
    )
  )
)

;Attempts to append a move downwards for the specified piece.
;If it is possible, appends the move.
;If it is not possible, returns the moves list unaltered.
(define append-move-left
  (lambda (board colIndex rowIndex origCol origRow movesList)
    (if
      (and
        (> (- colIndex 2) -1)
        (eqv? '- (vector-ref (vector-ref board rowIndex) (- colIndex 2)))
        (not (eqv? '- (vector-ref (vector-ref board rowIndex) (- colIndex 1))))
      )
      (append-move-left board (- colIndex 2) rowIndex origCol origRow
                        (append movesList (list (list (list origCol origRow) (list (- colIndex 2) rowIndex)))))
      movesList
    )
  )
)

;Returns a list of all possible game boards created by doing all possible moves
;for the current player
(define child-boards
  (lambda (board whose-turn)
    (map (lambda (mv) (move (board-copy board) mv)) (possible-moves board whose-turn 0 '()))
  )
)

;Finds the maximum value in a list
(define list-max
  (lambda (list)
    (if (= (length list) 1)
        (car list)
        (max (car list) (list-max (cdr list)))
    )
  )
)

;Finds the minimum value in a list
(define list-min
  (lambda (list)
    (if (= (length list) 1)
        (car list)
        (min (car list) (list-min (cdr list)))
    )
  )
)

;Applies the minimax algorithm to the specified game board to find the best move
;board: current game board
;depth: how deep to go in minimax computation tree
;whose-turn: 'x or 'o
(define minimax
  (lambda (board depth whose-turn us opp)
      (cond
        ((leaf board whose-turn us opp) (evaluate board whose-turn us opp))
        ((= depth 0) (evaluate board whose-turn us opp))
        (else
          (if (eq? whose-turn us)
              (list-max (map (lambda (b) (minimax b (- depth 1) opp us opp)) (child-boards board whose-turn)))
              (list-min (map (lambda (b) (minimax b (- depth 1) us us opp)) (child-boards board whose-turn)))
          )
        )
      )
  )
)

;Finds the best move in a list of moves according to their evaluation values
(define move-max
  (lambda (list)
    (if (= (length list) 1)
        (car list)
        (if (> (caar list) (car (move-max (cdr list))))
            (car list)
            (move-max (cdr list))
        )
    )
  )
)

;Gives the move with the best chance of winning.
;Board: current game board
;Depth: how deep to go in the minimax computation
(define (bestmove board depth piece us opp)
  (cadr (move-max (map (lambda (mv) (list (minimax (move (board-copy board) mv) depth opp us opp) mv)) (possible-moves board piece 0 '()))))
)

(define (secondmovepossibilities firstmove)
  (append
    (if (> (car firstmove) 0)
        (list (list (- (car firstmove) 1) (cadr firstmove)))
        '()
    )
        (append
          (if (< (car firstmove) 7)
              (list (list (+ (car firstmove) 1) (cadr firstmove)))
              '()
          )
              (append
                (if (> (cadr firstmove) 0)
                    (list (list (car firstmove) (- (cadr firstmove) 1)))
                    '()
                )
                  (if (< (cadr firstmove) 7)
                     (list (list (car firstmove) (+ (cadr firstmove) 1)))
                     '()
                  )
              )
        )
  )
)

(define (secondmovechooser board firstmove depth)
  (cadr (move-max (map (lambda (mv) (list (minimax (putpiece (board-copy board) (car mv) (cadr mv) '-) depth 'x 'o 'x) mv))
                       (secondmovepossibilities firstmove))))
)

(define (makeboard)
  (vector
     (list->vector '(X O X O X O X O))
     (list->vector '(O X O X O X O X))
     (list->vector '(X O X O X O X O))
     (list->vector '(O X O X O X O X))
     (list->vector '(X O X O X O X O))
     (list->vector '(O X O X O X O X))
     (list->vector '(X O X O X O X O))
     (list->vector '(O X O X O X O X))))

(define (putpiece board x y p)
  (vector-set! (vector-ref board y) x p)
  board)

(define (printwell arr)
  (display (string-append "  1 2 3 4 5 6 7 8" "\n"))
  (printweller (vector->list arr) 8))

(define (printweller arr r)
  (if (vector? arr)
      (let ((l (vector->list arr))) (if (if (display r) (display " ")) (printweller l r)))
      (if (list? arr)
          (if (= 0 (length arr)) (display "\n") (if (printweller (car arr) r) (printweller (cdr arr) (- r 1))))
          (if (display arr) (display " "))))
  #t)

(define (printmove dp)
  (display (string-append "Move piece at <"
                          (number->string (+ (caar dp) 1))
                          ","
                          (number->string (- 8 (cadar dp)))
                          "> to location <"
                          (number->string (+ (caadr dp) 1))
                          ","
                          (number->string (- 8 (cadadr dp)))
                          ">"
                          "\n"))
  dp)

(define (printinitmove m)
  (display (string-append "Remove piece at <"
                          (number->string (+ (car m) 1))
                          ","
                          (number->string (- 8 (cadr m)))
                          ">"
                          "\n"))
  m)

(define (clearline board x1 y1 x2 y2)
  (if (and (= x1 x2) (= y1 y2))
      (if (putpiece board x1 y1 '-)
       board)
      (let (
            (vl (if (= x1 x2)
                    (if (< y1 y2) y1 y2)
                    (if (< x1 x2) x1 x2)))
            (vg (if (= x1 x2)
                    (if (>= y1 y2) y1 y2)
                    (if (>= x1 x2) x1 x2)))
            (cl (if (= x1 x2)
                    (if (< y1 y2) x1 x2)
                    (if (< x1 x2) y1 y2)))
            (cg (if (= x1 x2)
                    (if (>= y1 y2) x1 x2)
                    (if (>= x1 x2) y1 y2))))
        (clearline (putpiece board (if (= x1 x2) cl vl) (if (= x1 x2) vl cl) '-)
                   (if (= x1 x2) cl (+ 1 vl))
                   (if (= x1 x2) (+ 1 vl) cl)
                   (if (= x1 x2) cg vg)
                   (if (= x1 x2) vg cg)))))


(define (firstmove)
  (let ((f (if (display (string-append "Our first move: " "\n")) (inputnum "" "firstmove"))))
    (let ((s (if (display (string-append "Their second move: " "\n")) (inputnum "" "firstmove"))))
      (play (putpiece (putpiece (makeboard) (car f) (cadr f) '-) (car s) (cadr s) '-) 1 'X 'O))))

(define (secondmove)
  (let ((f (if (display (string-append "Their first move: " "\n")) (inputnum "" "secondmove"))))
    (let ((b (putpiece (makeboard) (car f) (cadr f) '-)))
      (let ((s (printinitmove (secondmovechooser b f 4))))
        (play (putpiece b (car s) (cadr s) '-) 2 'O 'X)))))

(define (init turn)
  (if (= turn 1) (firstmove) (secondmove)))

(define (move board dp)
  (let ((p (vector-ref (vector-ref board (cadar dp)) (caar dp))))
    (putpiece (clearline board (caar dp) (cadar dp) (caadr dp) (cadadr dp)) (caadr dp) (cadadr dp) p)))

(define (inputnum n caller)
   (list (cond
           ((display (string-append "X" n ": "))
            (let ((input (read)))
              (if (or (< input 1) (> input 8)) (index-out-of-bounds n caller) (- input 1)))))
         (cond
           ((display (string-append "Y" n ": "))
            (let ((input (read)))
              (if (or (< input 1) (> input 8)) (index-out-of-bounds n caller) (- 8 input)))))))

(define (index-out-of-bounds n caller)
  (display "Invalid move. Values must be from 1 to 8.")(newline)
  (cond
    ((eqv? "secondmove" caller) (secondmove))
    ((eqv? "firstmove" caller) (firstmove))
    ;((eqv? "input" caller) (input))
    ))

;Prints an error message if the given input is not a valid first move.
(define (first-move-check)
  (let ((input (read)))
    (if (not (or (= input 1) (= input 8) (= input 4) (= input 5)))
      ((display "Invalid entry. Acceptable first moves are <1, 8>, <8, 1>, <4, 5>, and <5, 4>.")(newline)))))

;Prints an error message if the move made is not possible.
(define (jump-check board whose-turn)
  (if (not (contains (possible-moves board whose-turn 0 '()) (input)))
      ((display "Invalid move. Pieces have to move to empty squares and can only move by jumping orthogonal pieces.")(newline))))

;Prints an error message if the first question is not given yes or no.
(define (invalid-answer)
  (if (not (or (equal? READ 'Yes) (equal? READ 'No)))
      (display "Invalid entry. Please enter Yes if you are the first player and No if you are the second player.")))

(define (input)
  (list
   (inputnum "1" "input")
   (inputnum "2" "input")))

(define (nomoves board turn)
  (if (= (length (possible-moves board turn 0 '())) 0) #t #f))

(define (play board turn us opp)
  (printwell board)
  (if (= turn 1)
      (if (nomoves board us) #f (if (display (string-append "Our Turn: " "\n")) (play (move board (printmove (bestmove board 4 us us opp))) 2 us opp)))
      (if (nomoves board opp) #t (if (display (string-append "Opponent's Turn: " "\n")) (play (move board (input)) 1 us opp)))))

(if
 (let ((turn (if (equal? (if (display "Do we start? ('Yes' or 'No') ") (read)) 'Yes) 1 2)))
   (init turn))
 (display "We won!")
 (display "We lost."))