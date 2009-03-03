;Exercise 1.27: Demonstrate that the Carmichael numbers listed in unde\ufb01ned
;[Footnote 1-47], page unde\ufb01ned really do fool the Fermat test. That is, write
;a procedure that takes an integer n and tests whether a ^n is congruent to
;a modulo n for every a <n, and try your procedure on the given Carmichael
;numbers.

(define (fast-expt-iter b n acc)
  (cond ((= n 0) acc)
        ((even? n) (fast-expt-iter (square b) (/ n 2) acc))
        (else (fast-expt-iter b (- n 1) (* b acc)))))

(define (square x) (* x x))

(define (fast-expt b n) (fast-expt-iter b n 1))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (total-fermat-test n)
  (define (try-it a)
    (if (< a n)
        (if (= (expmod a n n) a)
            (try-it (+ 1 a))
            false)
        true))
  (try-it 1))

;; working prime test
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

;; test carmichael numbers

(define (test-carmichael n)
  (and (total-fermat-test n) (not (prime? n))))

;; Results:
; > (test-carmichael 561)
; #t
; > (test-carmichael 1105)
; #t
; > (test-carmichael 1729)
; #t
;
; Which means that 561, 1105 and 1729 really do fool fermat's test.
