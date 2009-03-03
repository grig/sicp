(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (inc x) (+ x 1))

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (offset k) (* k h))
  (define (coeff k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (func k) (* (coeff k) (f (+ a (offset k)))))                  
  (* (/ h 3) (sum func 0 inc n)))

;; tests

(define (cube x) (* x x x))

(define (test n)
  (list (- 1/4 (integral cube 0 1 (/ 1.0 n)))
        (- 1/4 (simpsons-integral cube 0.0 1.0 n))))