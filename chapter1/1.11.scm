;Exercise 1.11: A function f is de\ufb01ned by the rule that f (n ) = n if n <3 and f (n )
;= f (n - 1) + 2f (n - 2) + 3f (n - 3) if n >= 3. Write a procedure that computes f
;by means of a recursive process. Write a procedure that computes f by means
;of an iterative process.

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; f(1) = 1; f(2) = 2; f(3) = 2 + 2*1 + 3*0 = 4; f(4) = 4 + 2*2 + 3*1 = 11
; (x, y, z) -> (y, z, z + 2*y + 3*x)


(define (f-iter x y z count)
  (if (= count 0)
      z
      (f-iter y z (+ z (* 2 y) (* 3 x)) (- count 1))))

(define (f1 n)
  (f-iter 0 1 2 (- n 2)))