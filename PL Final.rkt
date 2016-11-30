;#lang R5RS

(define (evaluate board)
  (cond
    ((= (number-moveable board 'o 0 0) 0)
     100)
    ((= (number-moveable board 'x 0 0) 0)
     0)
    (else (/ (number-moveable board 'x 0 0)
     (number-moveable board 'o 0 0)))))

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

(define (bestmove board)
  (list (list 0 0) (list 2 0)))

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
    (putpiece (putpiece (makeboard) x1 y1 '-) x2 y2 '-))

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
      (if (nomoves board) #f (play (move board (printmove (bestmove board))) (not turn)))
      (if (nomoves board) #t (play (move board (input)) (not turn)))))

(if (play (init
           (if (display "X1: ") (read))
           (if (display "Y1: ") (read))
           (if (display "X2: ") (read))
           (if (display "Y2: ") (read)))
      (if (equal? (if (display "X or O? ") (read)) "X") #t #f))
    (display "You won!")
    (display "You lost."))