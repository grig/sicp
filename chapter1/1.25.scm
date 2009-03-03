(define runtime current-inexact-milliseconds)

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime n (- (runtime) start-time))
      false))


(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)

(define (square x) (* x x))
;; fermat test

;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (square (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m))
;                    m))))

(define (fast-expt-iter b n acc)
  (cond ((= n 0) acc)
        ((even? n) (fast-expt-iter (square b) (/ n 2) acc))
        (else (fast-expt-iter b (- n 1) (* b acc)))))

(define (fast-expt b n) (fast-expt-iter b n 1))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;;; Solution

(define (search-for-primes current howmuch)
  (if (= howmuch 0)
      current
      (if (timed-prime-test current)
          (search-for-primes (+ 2 current) (- howmuch 1))
          (search-for-primes (+ 2 current) howmuch))))

;; Result
; > (search-for-primes 1001 3)
;1009 *** 0.43798828125
;1013 *** 0.517822265625
;1019 *** 0.453857421875

; so, this method is 10 times slower than the one in ex1.24.
; new version of expmod takes same number of steps, but numbers involved are much higher in Alyssa's version. For example, if (random) returns 1000, we are using numbers on the scale of 1000^1000.
