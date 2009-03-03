(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (if (filter a)
            (iter (next a) (combiner (term a) acc))
            (iter (next a) acc))))
  (iter a null-value))

(define (inc n) (+ n 1))
(define (identity x) x)

; the sum of the squares of the prime numbers in the interval a to b (assuming
; that you have a prime? predicate already written)

;; prime? 
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next divisor)
  (if (= divisor 2)
      3
      (+ divisor 2)))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x) (* x x))
(define (prime? n) 
  (= n (smallest-divisor n)))

;; solution

(define (sum-of-squares-of-prime-numbers a b)
  (filtered-accumulate prime? + 0 square a inc b))

; b. the product of all the positive integers less than n that are relatively
; prime to n (i.e., all positive integers i < n such that GCD (i,n ) = 1).

(define (remainder a b)
  (if (< a b)
      a
      (remainder (- a b) b)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; solution

(define (product-of-relative-primes n)
  (define (relatively-prime? a)
    (= (gcd a n) 1))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc n))
