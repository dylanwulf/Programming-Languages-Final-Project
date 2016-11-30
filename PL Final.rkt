
(define (makeboard)
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