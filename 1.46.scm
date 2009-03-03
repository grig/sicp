(define (iterative-improve good-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (good-enough? next guess)
          next
          (try next))))
      
  (lambda (guess) (try guess)))

(define (sqrt x)
  (define (good-enough? guess next) (< (abs (- (square guess) x)) 0.000001))
  (define (improve guess) (average guess (/ x guess)))  
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f initial-guess)
  (define (good-enough? y1 y2) (< (abs (- y1 y2)) 0.0000001))
  (define (improve guess) (f guess)) 
  ((iterative-improve good-enough? improve) initial-guess))
                                
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
