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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

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

;; results 
;> (search-for-primes 1001 3)
;1009 *** 0.0400390625
;1013 *** 0.0400390625
;1019 *** 0.0400390625
;> (search-for-primes 10001 3)
;10007 *** 0.048828125
;10009 *** 0.080078125
;10037 *** 0.046875
;> (search-for-primes 100001 3)
;100003 *** 0.100830078125
;100019 *** 0.069091796875
;100043 *** 0.0810546875
;> (search-for-primes 1000001 3)
;1000003 *** 0.09716796875
;1000033 *** 0.0859375
;1000037 *** 0.089111328125

; log(x*n)/log(n) = y
; log(x*n) = y*log(n)
; x*n = n^y
; x = n^(y - 1)
; y = log_n(x) + 1
; n = 1000, x = 1000 => y = log_1000(1000) + 1 = 2
; a test: 
; > ( / 0.089111328125 0.046875)
; 1.9010416666