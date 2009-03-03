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

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (add-interval x (negate-interval y)))

(define (negate-interval x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))

;Exercise 2.9: The width of an interval is half of the difference between its
;upper and lower bounds. The width is a measure of the uncertainty of the
;number specified by the interval. For some arithmetic operations the width
;of the result of combining two intervals is a function only of the widths of
;the argument intervals, whereas for others the width of the combination is not
;a function of the widths of the argument intervals. Show that the width of
;the sum (or difference) of two intervals is a function only of the widths of the
;intervals being added (or subtracted). Give examples to show that this is not
;true for multiplication or division.


;x = (lx, ux), wx = (ux-lx)/2
;y = (ly, uy), wy = (uy-ly)/2
;
;w(x+y) = w(lx+ly, ux+uy) = (ux + uy - lx - ly)/2 = (ux-lx)/2 + (uy - ly)/2 = wx + wy.
;
;w(x-y) = w(lx-uy, ux-ly) = ((ux-ly)-(lx-uy))/2 = (ux-lx)/2 + (uy-ly)/2 = wx + wy

(define (width x) (half (- (upper-bound x) (lower-bound x))))
(define (half x) (/ x 2))

;; however, 7/2 = w([1,3] x [2,3]) != w([2,4]x[2,3]) = 4, despite the width of the first interval has not changed, only its bounds have.