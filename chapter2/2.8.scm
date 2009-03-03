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

;; exercise 2.8
;; Using reasoning analogous to Alyssa's, describe how the difference 
;; of two intervals may be computed. De\ufb01ne a corresponding subtraction
;; procedure, called sub-interval.

;; if l_a <= x <= u_a; l_b <= y <= u_b; then 
;; -u_b <= -y <= -l_b, and
;; l_a - u_b <= x - y <= u_a - l_b

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; the same, implemented in terms of addition and negation:
(define (sub-interval1 x y)
  (add-interval x (negate-interval y)))

(define (negate-interval x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))