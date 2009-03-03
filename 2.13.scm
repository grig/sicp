(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval x y) (cons x y))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (add-interval x (negate-interval y)))

(define (negate-interval x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))

(define (div-interval x y)
  (if (spans? y 0)
      (error "denominator must not span 0")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (spans? i x)
  (and (<= (lower-bound i) x) (>= (upper-bound i) x)))

(define (error msg)
  (display msg))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c tolerance)
  (let ((width (* c (/ tolerance 100.0))))
    (make-center-width c width)))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))

;Exercise 2.13: Show that under the assumption of small percentage tolerances
;there is a simple formula for the approximate percentage tolerance of the 
;product of two intervals in terms of the tolerances of the factors. You may
;simplify the problem by assuming that all numbers are positive.
 
;; c_x(1 - t_x) < x < c_x(1 + t_x)
;; c_y(1 - t_y) < y < c_y(1 + t_y)
;; assuming t_x, t_y << 1; c_x, c_y > 0:
;; c_x*c_y(1-t_x)(1-t_y) < xy < c_x*c_y*(1+t_x)(1+t_y)
;; c_x*c_y(1 - tx - t_y - t_x*t_y) < xy < c_x*c_y(1 + t_x + t_y + t_x*t_y)
;; discarding t_x*t_y we get
;; c_x*c_y*(1 - (t_x+t_y)) < xy < c_x*c_y(1 + (t_x + t_y))
;;
;; => t_xy = t_x + t_y if t_x, t_y << 1
;
;> (define x (make-center-percent 3 0.1))
;> (define y (make-center-percent 5 0.1))
;> (define xy (mul-interval x y))
;> (percent xy)
;0.19999980000020165