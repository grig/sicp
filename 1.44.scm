(define dx 0.000001)

(define (smooth f)
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (average a b c) (/ (+ a b c) 3))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; definition of repeated
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))