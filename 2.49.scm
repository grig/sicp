;Exercise 2.49. Use segments->painter to define the following primitive painters:
;a. The painter that draws the outline of the designated frame.

(define (ms x1 y1 x2 y2)
  (make-segment (make-vect x1 y1) (make-vect x2 y2)))

(segments->painter (list
                    (ms 0 0 0 1)
                    (ms 0 1 1 1)
                    (ms 1 1 1 0)
                    (ms 1 0 0 0)))

;b. The painter that draws an "X" by connecting opposite corners of the frame.

(segments->painter (list
                    (ms 0 0 1 1)
                    (ms 0 1 1 0)))

;c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

(segments->painter (list (ms 0 0.5 0.5 1)
                         (ms 0.5 1 1 0.5)
                         (ms 1 0.5 0.5 0)
                         (ms 0.5 0 0 0.5)))

;d. The wave painter.

;; wtf?