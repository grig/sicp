(define (double f)
  (lambda (x)
    (f (f x))))

; tests
(define (inc x) (+ 1 x))

; (((double (double double)) inc) 5)
; (((double (lambda (f) (double (double f)))) inc 5)
; (((lambda(g) (double (double (double (double g))))) inc) 5)
; ((double (double (double (double inc)))) 5)
; ((double (double (double add2))) 5)
; ((double (double add4)) 5)
; ((double add8) 5)
; (add16 5)
; 21