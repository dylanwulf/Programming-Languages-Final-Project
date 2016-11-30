
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

(define (removepiece board x y)
    (vector-set! (vector-ref board y) x '-))

(define (game x1 y1 x2 y2)
  (let ((board (makeboard)))
    (removepiece board x1 y1)
    (removepiece board x2 y2)
    board))

(define (evaluate board)
  (count-total (map vector->list (vector->list board)) 0))

(define (count-total board total)
  (cond ((null? board) total)
        (else (count-total (cdr board) (+ total (count (car board) 'x 0))))))

(define (count line type total)
  (cond
    ((null? line) total)
    ((eqv? (car line) type) (count (cdr line) type (+ total 1)))
    (else (count (cdr line) type total))))