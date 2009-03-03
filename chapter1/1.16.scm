;Exercise 1.16: Design a procedure that evolves an iterative exponentiation pro-
;cess that uses successive squaring and uses a logarithmic number of steps, as
;does fast-expt. (Hint: Using the observation that (b ^(n /2))^2 = (b ^2)^(n /2),
;keep, along with the exponent n and the base b, an additional state variable
;a, and de\ufb01ne the state transformation in such a way that the product a b ^n is
;unchanged from state to state. At the beginning of the process a is taken to be
;1, and the answer is given by the value of a at the end of the process. In gen-
;eral, the technique of de\ufb01ning an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))

; b^9 = ((b^2)^2)^2*b
; (b = 2, n = 9, acc = 1)
; (b = 2, n = 8, acc = 2)
; (b = 4, n = 4, acc = 2)
; (b = 16, n = 2, acc = 2)
; (b = 256, n = 1, acc = 2)
; (b = 256, n = 0, acc = 512)

; on each step we determine if we want to square or multiply:
; (b=2, n = 5, )

(define (fast-expt-iter b n acc)
  (display (list b n acc))
  (cond ((= n 0) acc)
        ((even? n) (fast-expt-iter (square b) (/ n 2) acc))
        (else (fast-expt-iter b (- n 1) (* b acc)))))

(define (fast-expt1 b n) (fast-expt-iter b n 1))