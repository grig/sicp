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

;Exercise 2.14: Demonstrate that Lem is right. Investigate the behavior of the
;system on a variety of arithmetic expressions. Make some intervals A and B,
;and use them in computing the expressions A/A and A/B. You will get the most
;insight by using intervals whose width is a small percentage of the center
;value.
;Examine the results of the computation in center-percent form (see 
;[Exercise 2-12]).

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;x = (c - tc, c + tc)
;1/x = 1/(c-tc, c+tc) = (1/(c+tc), 1/(c-tc))
;
;c(1/x) = 1/2*(1/(c+tc) + 1/(c - tc)) = 1/2*((c - tc + c + tc)/(c^2 - t^2c^2))
;= c/(c^2-t^2c^2) = 1/(c - t^2c)
;
;w(1/x) = 1/2*(1/(c-tc) - 1/(c+tc)) = 1/2*(c+tc-c+tc)/(c^2-c^2t^2) = 
;= tc/c^2(1-t^2) = t/c(1 - t^2) ~= t/c
;
;t(1/x) = w(1/x)/c(1/x) = t/c(1-t^2) * c(1-t^2) = t(x)
;
;t(x/x) = t(x*1/x) = t(x) + t(1/x) = 2t(x)
;t(x/y) = t(x) + t(y)
;
;c(x+y) = c((c_x-t_xc_x+c_y-t_yc_y, c_x+t_xc_x+c_y+t_yc_y)) = 
;1/2(2c_x + 2c_y) = c_x+c_y
;w(x+y) = 1/2(2t_xc_x + 2t_yc_y) = t_xc_x + t_yc_y
;t(x+y) = w(x+y)/c(x+y) = (t_xc_x + t_yc_y)/(c_x+c_y)
;если t_x <= t_y, то t_x <= t(x+y) <= t_y
;
;t(par1 a a) = t(a*a/(a+a)) = t(a*a) + t(a + a) = t(a) + t(a) + t(a + a) = 3t(a)
;
;t(par2 a a) = t(1/(1/a + 1/a)) = t(1) + t(1/a + 1/a) = t(1/a + 1/a) = t(1/a) = t(a)
;

; hence, par1 is 3 times worse than par2.