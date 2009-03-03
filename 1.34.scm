; Exercise 1.34: Suppose we de\ufb01ne the procedure
(define (f g)
  (g 2))
;Then we have
;     (f square)
;     4
;     (f (lambda (z) (* z (+ z 1))))
;     6
;What happens if we (perversely) ask the interpreter to evaluate the combination
;(f f)? Explain.

; first, f is evaluated (twice) to the value of 
; (lambda (g) (g 2))

; so, (f f) evaluates to 
; ((lambda (g) (g 2)) (lambda (h) (h 2))) =>
; ((lambda (h) (h 2)) 2) =>
; (2 2) which is an error