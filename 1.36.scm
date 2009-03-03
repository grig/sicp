;Exercise 1.36: Modify fixed-point so that it prints the sequence of approxi-
;mations it generates, using the newline and display primitives shown in un-
;defined [Exercise 1-22], page undefined . Then find a solution to x ^x = 1000
;by finding a fixed point of x |-> log(1000)/log(x ). (Use Scheme's primitive
;log procedure, which computes natural logarithms.) Compare the number of
;steps this takes with and without average damping. (Note that you cannot
;start fixed-point with a guess of 1, as this would cause division by log(1) =
;0.)

(define tolerance 0.00000001)

(define (close-enough? guess next)
  (< (abs (- guess next)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess count)
    (display (list guess count))
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ 1 count)))))
  (try first-guess 1))

(define (solve-x-to-the-power-of-x n)
  (fixed-point (lambda (x) (/ (log n) (log x))) 2.0))

(define (average x y) (/ (+ x y) 2))

(define (solve-x-to-the-power-of-x-with-average-damping n)
  (fixed-point (lambda (x) (average x (/ (log n) (log x)))) 2.0))

;; results: with tolerance of 0.00000001 solving for x took 52 steps without average damping, and only 14 with one.