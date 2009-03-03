(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (square x) (* x x))   

(define (tan-cf x k)
  (let ((minus-x-squared (- (square x))))
    (cont-frac (lambda (i) (if (= i 1) x minus-x-squared))
               (lambda (i) (- (* 2 i) 1))
               k)))
