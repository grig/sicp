;Exercise 1.22: Most Lisp implementations include a primitive called runtime
;that returns an integer that speci\ufb01es the amount of time the system has
;been running (measured, for example, in microseconds). The following
;timed-prime-test procedure, when called with an integer n, prints n and
;checks to see if n is prime. If n is prime, the procedure prints three asterisks
;followed by the amount of time used in performing the test.
(define runtime current-inexact-milliseconds)

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))


(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)
;Using this procedure, write a procedure search-for-primes that checks the
;primality of consecutive odd integers in a speci\ufb01ed range. Use your procedure
;to \ufb01nd the three smallest primes larger than 1000; larger than 10,000; larger
;than 100,000; larger than 1,000,000. Note the time needed to test each prime.
;Since the testing algorithm has order of growth of [theta] ([sqrt] (n )), you should
;expect that testing for primes around 10,000 should take about [sqrt] (10) times
;as long as testing for primes around 1000. Do your timing data bear this out?
;How well do the data for 100,000 and 1,000,000 support the [sqrt] (n ) prediction?
;Is your result compatible with the notion that programs on your machine run
;in time proportional to the number of steps required for the computation?


(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x) (* x x))
(define (prime? n) 
  (= n (smallest-divisor n)))

;;;; Solution

(define (search-for-primes current howmuch)
  (if (= howmuch 0)
      current
      (if (timed-prime-test current)
          (search-for-primes (+ 2 current) (- howmuch 1))
          (search-for-primes (+ 2 current) howmuch))))