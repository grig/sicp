(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;Exercise 2.34: Evaluating a polynomial in x at a given value of x can be for-
;mulated as an accumulation. We evaluate the polynomial
;      a_n r^n | a_(n-1) r^(n-1) + ... + a_1 r + a_0
;using a well-known algorithm called Horner\u2019s rule , which structures the com-
;putation as
;      (... (a_n r + a_(n-1)) r + ... + a_1) r + a_0
;In other words, we start with a n, multiply by x, add a (n-1), multiply by x,
;and so on, until we reach a 0.16
;Fill in the following template to produce a procedure that evaluates a polyno-
;mial using Horner\u2019s rule. Assume that the coe\ufb03cients of the polynomial are
;arranged in a sequence, from a 0 through a n.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))
       
;For example, to compute 1 + 3x + 5x ^3 + x ^(5) at x = 2 you would evaluate
;       (horner-eval 2 (list 1 3 0 5 0 1))

;; => 79
