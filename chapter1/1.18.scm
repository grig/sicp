;Exercise 1.18: Using the results of [Exercise 1-16] and [Exercise 1-17],
;devise a procedure that generates an iterative process for multiplying two 
;integers in terms of adding, doubling, and halving and uses a logarithmic
;number of steps.

(define (mul-iter a b acc)
  (cond ((= b 0) acc)
        ((even? b) (mul-iter (double a) (halve b) acc))
        (else (mul-iter a (- b 1) (+ a acc)))))

(define (double a) (+ a a))
(define (halve a) (/ a 2))