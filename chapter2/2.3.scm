;; two diagonal points
(define (make-rect p1 p2) (make-segment p1 p2))
(define (width r)
  (abs (- (x-point (start-segment r)) (x-point (end-segment r)))))
(define (height r)
  (abs (- (y-point (start-segment r)) (y-point (end-segment r)))))

;; dumb: store only width and height as that is what currenly needed for calculating area and perimeter.
(define (make-rect width height) (cons (abs width) (abs height)))
(define (width r) (car r))
(define (height r) (cdr r))

(define (perimeter rect)
  (+ (* 2 (width rect)) (* 2 (height rect))))

(define (area rect)
  (* (width rect) (height rect)))


;; points & segments
(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (average a b) (/ (+ a b) 2))
(define (midpoint s)
  (let ((start (start-segment s))
        (end (end-segment s)))   
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

