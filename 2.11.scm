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

(define (make-interval a b) (cons a b))

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

; Exercise 2.12
;;
;; 0 < ly < y < uy
;;    0 < lx < x < ux => lx*ly < x*y < ux*uy
;;    lx < 0 < x < ux => lx*uy < x*y < ux*uy
;;    lx < x < ux < 0 => lx*uy < x*y < ux*ly
;; 
;; ly < 0 < y < uy
;;    0 < lx < x < ux => ly * ux < x*y < uy * ux
;;    lx < 0 < x < ux => min(lx * uy, ly*ux) < x*y < max (ly*lx, uy*ux)
;;    lx < x < ux < 0 => lx*uy < x*y < lx*ly
;;
;; ly < y < uy < 0
;;    0 < lx < x < ux => ux*ly < x*y < lx*uy
;;    lx < 0 < x < ux => ux*ly < x*y < lx*ly
;;    lx < x < ux < 0 => ux*uy < x*y < lx*ly


(define (mul-interval1 x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((>= ly 0)
           (cond ((>= lx 0) (make-interval (* lx ly) (* ux uy)))
                 ((< ux 0) (make-interval (* lx uy) (* ux ly)))
                 (else (make-interval (* lx uy) (* ux uy)))))
          ((< uy 0)
           (cond ((>= lx 0) (make-interval (* ux ly) (* lx uy)))
                 ((< ux 0) (make-interval (* ux uy) (* lx ly)))
                 (else (make-interval (* ux ly) (* lx ly)))))
          (else
           (cond ((>= lx 0) (make-interval (* ly ux) (* uy ux)))
                 ((< ux 0) (make-interval (min (* lx uy) (* ly ux)) 
                                          (max (* ly lx) (* uy ux))))
                 (else (make-interval (* lx uy) (* lx ly))))))))
          

