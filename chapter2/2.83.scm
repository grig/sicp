;Exercise 2.83. Suppose you are designing a generic arithmetic system for dealing with the tower of types
;shown in figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure
;that raises objects of that type one level in the tower. Show how to install a generic raise operation that
;will work for each type (except complex).

(define (raise-integer n)
  (make-rational (contents n) 1))

(define (raise-rational r)
  (make-real (/ (numerator r) (denominator r))))

(define (raise-real x)
  (make-complex x 0))

;; generic raise operation
(define (raise x) ((get 'raise (type-tag x)) (contents x)))

;; specializations of generic raise
(put 'raise '(scheme-number) raise-integer) ; install-scheme-number-package
(put 'raise '(rational) raise-rational)     ; install-rational-package
(put 'raise ('real) raise-real)             ; install-real-package
