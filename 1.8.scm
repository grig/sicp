;Exercise 1.8: Newton\u2019s method for cube roots is based on the fact that if y is
;an approximation to the cube root of x, then a better approximation is given
;by the value
;      x/y^2 + 2y
;      ----------
;           3
;Use this formula to implement a cube-root procedure analogous to the square-
;root procedure. (In section unde\ufb01ned [1-3-4], page unde\ufb01ned we will see how
;to implement Newton\u2019s method in general as an abstraction of these square-root
;and cube-root procedures.)


(define (cube-root-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (cube-root-iter (improve guess x) guess x)))

(define (good-enough? guess old-guess) 
  (< (abs (- guess old-guess)) (* 0.001 guess)))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (improve guess x)
  (average guess guess (/ x (square guess))))

(define (average x y z)
  (/ (+ x y z) 3))

; (define (sqrt x) (sqrt-iter 1.0 x))

(define (cube-root x) (cube-root-iter 1.0 0.0 x))
