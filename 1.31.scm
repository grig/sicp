(define (recursive-product term a next b)
  (if (> a b)
      1
      (* (term a) (recursive-product term (next a) next b))))

(define (iterative-product term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (* acc (term a)))))
  (iter a 1))

(define product iterative-product)

(define (inc n) (+ n 1))
(define (identity x) x)

(define (approx-pi n)
  (define (foo k)
    (if (even? k)
        (+ 2.0 k)
        (+ 2.0 (+ k 1))))

  (define (bar k)
    (if (even? k)
        (+ 3.0 k)
        (+ 3.0 (- k 1))))
  
  (define (quot k)
    (/ (foo k) (bar k)))
  
  (* 4 (product quot 0 inc n)))

(define (factorial n) (product identity 1 inc n))
