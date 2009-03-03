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

;;;; Solution

(define (search-for-primes current howmuch)
  (if (= howmuch 0)
      current
      (if (timed-prime-test current)
          (search-for-primes (+ 2 current) (- howmuch 1))
          (search-for-primes (+ 2 current) howmuch))))

;; results:
;; search start  w/o optimization w/optimization  ratio
;; 1000000       2.95369466       1.7099609375    1.7273462783
;; 10^9          80.341064453125  45.966064453125 1.74783
;; 10^12         5848.474609375   3068.2287597656 1.9061403393
;;
;; As we can see, the ratio is almost 2. It might be lower because + is a primitive function while (next) isn't and as such has some overhead.
