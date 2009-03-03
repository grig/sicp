(define tolerance 0.00001)

(define (close-enough? guess next)
  (< (abs (- guess next)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;Exercise 1.35: Show that the golden ratio [phi] (section [1-2-2],
;) is a fixed point of the transformation x |-> 1 + 1/x, and use
;this fact to compute [phi] by means of the fixed-point procedure.

; phi is defined as a root of equation
; phi^2 = phi + 1
; phi = phi^2 - 1
; or, dividing by phi,
; 1 = phi - 1/phi
; phi = 1 + 1/phi, q.e.d.

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

