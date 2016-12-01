#lang R5RS

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

(define (putpiece board x y p)
  (vector-set! (vector-ref board y) x p)
  board)

(define (printwell arr)
  (if (vector? arr) (let ((l (vector->list arr))) (printwell l))
                      (if (list? arr)
                          (if (= 0 (length arr)) (display "\n") (if (printwell (car arr)) (printwell (cdr arr))))
                          (display arr)))
  #t)

(define (printmove dp)
  (display (string-append "Move piece at <"
                          (number->string (caar dp))
                          ","
                          (number->string (cadar dp))
                          "> to location <"
                          (number->string (caadr dp))
                          ","
                          (number->string (cadadr dp))
                          ">"
                          "\n"))
  dp)

(define (bestmove board)
  (list (list 0 0) (list 2 0)))

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
      (if (nomoves board) #f (if (display (string-append "Our Turn: " "\n")) (play (move board (printmove (bestmove board))) (not turn))))
      (if (nomoves board) #t (if (display (string-append "Opponent's Turn: " "\n")) (play (move board (input)) (not turn))))))

(if (play (init
           (if (display "X1: ") (read))
           (if (display "Y1: ") (read))
           (if (display "X2: ") (read))
           (if (display "Y2: ") (read)))
      (if (equal? (if (display "X or O? ") (read)) "X") #t #f))
    (display "You won!")
    (display "You lost."))
