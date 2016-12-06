;#lang R5RS

(define eval-count 0)
(define cut-count 0)
(define total-branches 0)
(define times-branched 0)

(define (avg-branch-factor)
  (/ total-branches times-branched))

(define (evaluate board whose-turn)
  (set! eval-count (+ eval-count 1))
  (cond
    ((= (number-moveable board 'o 0 0) 0)
     100)
    ((= (number-moveable board 'x 0 0) 0)
     0)
    (else (/ (number-moveable board 'x 0 0)
     (number-moveable board 'o 0 0)))))

(define (leaf board whose-turn)
  (or
   (and (= (number-moveable board 'o 0 0) 0) (eqv? whose-turn 'o))
   (and (= (number-moveable board 'x 0 0) 0) (eqv? whose-turn 'x))
  )
)

(define (count-total board total)
  (cond ((null? board) total)
        (else (count-total (cdr board) (+ total (count (car board) 'x 0))))))

(define (count line type total)
  (cond
    ((null? line) total)
    ((eqv? (car line) type) (count (cdr line) type (+ total 1)))
    (else (count (cdr line) type total))))

(define (number-moveable board type rowIndex total)
  (cond ((= rowIndex 8) total)
        (else (number-moveable board type (+ rowIndex 1) (+ total (number-moveable-row board type rowIndex 0 0))))))

(define (number-moveable-row board type rowIndex colIndex total)
  (cond
    ((> colIndex 7) total)
    ((eqv? type (vector-ref (vector-ref board rowIndex) colIndex))
     (cond
       ((and
         (< (+ rowIndex 2) 8)
         (eqv? '- (vector-ref (vector-ref board (+ rowIndex 2)) colIndex))
         (not (eqv? '- (vector-ref (vector-ref board (+ rowIndex 1)) colIndex))))
        ;(display rowIndex)(display colIndex)(display "First")(newline)
        (number-moveable-row board type rowIndex (+ colIndex 2) (+ total 1)))
       ((and
         (> (- rowIndex 2) -1)
         (eqv? '- (vector-ref (vector-ref board (- rowIndex 2)) colIndex))
         (not (eqv? '- (vector-ref (vector-ref board (- rowIndex 1)) colIndex))))
        ;(display rowIndex)(display colIndex)(display "Second")(newline)
        (number-moveable-row board type rowIndex (+ colIndex 2) (+ total 1)))
       ((and
         (< (+ colIndex 2) 8)
         (eqv? '- (vector-ref (vector-ref board rowIndex) (+ colIndex 2)))
         (not (eqv? '- (vector-ref (vector-ref board rowIndex) (+ colIndex 1)))))
        ;(display rowIndex)(display colIndex)(display "Third")(newline)
        (number-moveable-row board type rowIndex (+ colIndex 2) (+ total 1)))
       ((and
         (> (- colIndex 2) -1)
         (eqv? '- (vector-ref (vector-ref board rowIndex) (- colIndex 2)))
         (not (eqv? '- (vector-ref (vector-ref board rowIndex) (- colIndex 1)))))
        ;(display rowIndex)(display colIndex)(display "Fourth")(newline)
        (number-moveable-row board type rowIndex (+ colIndex 2) (+ total 1)))
       (else (number-moveable-row board type rowIndex (+ colIndex 2) total))))
    (else (number-moveable-row board type rowIndex (+ colIndex 1) total))))

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
       (possible-moves-row board whose-turn rowIndex (+ colIndex 2) (append-move-right board colIndex rowIndex (append-move-left board colIndex rowIndex (append-move-up board colIndex rowIndex (append-move-down board colIndex rowIndex movesList)))))
     )
    (else (possible-moves-row board whose-turn rowIndex (+ colIndex 1) movesList))))

;Attempts to append a move to the right for the specified piece.
;If it is possible, appends the move.
;If it is not possible, returns the moves list unaltered.
(define append-move-right
  (lambda (board colIndex rowIndex movesList)
    (if
      (and
        (< (+ rowIndex 2) 8)
        (eqv? '- (vector-ref (vector-ref board (+ rowIndex 2)) colIndex))
        (not (eqv? '- (vector-ref (vector-ref board (+ rowIndex 1)) colIndex)))
      )
      (append movesList (list (list (list colIndex rowIndex) (list colIndex (+ rowIndex 2)))))
      movesList
    )
  )
)

;Attempts to append a move to the left for the specified piece.
;If it is possible, appends the move.
;If it is not possible, returns the moves list unaltered.
(define append-move-left
  (lambda (board colIndex rowIndex movesList)
    (if
      (and
        (> (- rowIndex 2) -1)
        (eqv? '- (vector-ref (vector-ref board (- rowIndex 2)) colIndex))
        (not (eqv? '- (vector-ref (vector-ref board (- rowIndex 1)) colIndex)))
      )
      (append movesList (list (list (list colIndex rowIndex) (list colIndex (- rowIndex 2)))))
      movesList
    )
  )
)

;Attempts to append a move upwards for the specified piece.
;If it is possible, appends the move.
;If it is not possible, returns the moves list unaltered.
(define append-move-up
  (lambda (board colIndex rowIndex movesList)
    (if
      (and
        (< (+ colIndex 2) 8)
        (eqv? '- (vector-ref (vector-ref board rowIndex) (+ colIndex 2)))
        (not (eqv? '- (vector-ref (vector-ref board rowIndex) (+ colIndex 1))))
      )
      (append movesList (list (list (list colIndex rowIndex) (list (+ colIndex 2) rowIndex))))
      movesList
    )
  )
)

;Attempts to append a move downwards for the specified piece.
;If it is possible, appends the move.
;If it is not possible, returns the moves list unaltered.
(define append-move-down
  (lambda (board colIndex rowIndex movesList)
    (if
      (and
        (> (- colIndex 2) -1)
        (eqv? '- (vector-ref (vector-ref board rowIndex) (- colIndex 2)))
        (not (eqv? '- (vector-ref (vector-ref board rowIndex) (- colIndex 1))))
      )
      (append movesList (list (list (list colIndex rowIndex) (list (- colIndex 2) rowIndex))))
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
  (lambda (board depth whose-turn)
      (cond
        ((leaf board whose-turn) (evaluate board whose-turn))
        ((= depth 0) (evaluate board whose-turn))
        (else
          (if (eq? whose-turn 'X)
              (list-max (map (lambda (b) (minimax b (- depth 1) 'O)) (child-boards board whose-turn)))
              (list-min (map (lambda (b) (minimax b (- depth 1) 'X)) (child-boards board whose-turn)))
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
(define (bestmove board depth)
  (cadr (move-max (map (lambda (mv) (list (minimax (move (board-copy makeboard) mv) depth 'o) mv)) (possible-moves makeboard 'x 0 '()))))
)

(define makeboard
  (vector
     (list->vector '(O X O X O X O X))
     (list->vector '(X O X O X O X O))
     (list->vector '(O X O X O X O X))
     (list->vector '(X O X O X O X O))
     (list->vector '(O X O X O X O X))
     (list->vector '(X O X O X O X O))
     (list->vector '(O X O X O X O X))
     (list->vector '(X O X O X O X O))))

(define (putpiece board x y p)
  (vector-set! (vector-ref board y) x p)
  board)

(define (printwell arr)
  (if (vector? arr) (let ((l (vector->list arr))) (printwell l)
                      (if (list? arr)
                          (if (= 0 (length arr)) (display (newline)) (if (printwell (car arr)) (printwell (cdr arr))))
                          (display arr))))
  #t)

(define (printmove dp)
  (cons "Move piece at"
        (cons "<"
              (cons (caar dp)
                    (cons ","
                          (cons (cadar dp)
                                (cons "> to location <"
                                      (cons (caadr dp)
                                            (cons ","
                                                  (cons (cadadr dp) '(">")))))))))) dp)

(define (clearline board x1 y1 x2 y2)
  (if (and (= x1 x2) (= y1 y2)) board
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
    

(define (init x1 y1 x2 y2)
    (putpiece (putpiece makeboard x1 y1 '-) x2 y2 '-))

(define (move board dp)
  (let ((p (vector-ref (vector-ref board (caar dp)) (cadar dp))))
    (putpiece (clearline board (caar dp) (cadar dp) (caadr dp) (cadadr dp)) (caadr dp) (cadadr dp) p)))

(define (input)
  (list
   (list (if (display "X1: ") (read)) (if (display "Y1: ") (read)))
   (list (if (display "X2: ") (read)) (if (display "Y2: ") (read)))))

(define (nomoves board)
  #f)

(define (play board turn)
  (printwell board)
  (if turn
      (if (nomoves board) #f (play (move board (printmove (bestmove board 3))) (not turn)))
      (if (nomoves board) #t (play (move board (input)) (not turn)))))

(if (play (init
           (if (display "X1: ") (read))
           (if (display "Y1: ") (read))
           (if (display "X2: ") (read))
           (if (display "Y2: ") (read)))
      (if (equal? (if (display "X or O? ") (read)) "X") #t #f))
    (display "You won!")
    (display "You lost."))