(define (square x) (* x x))
;; fermat test

(define (expmod base exp m)
  (define (check-nontrivial-square-root result remainder-of-result-squared)
;    (display (list 'check-nontrivial-square-root-result result remainder-of-result-squared))
;    (newline)
;    
    (if (or (= result 1) (= result (- m 1)) (not (= remainder-of-result-squared 1)))
        remainder-of-result-squared
        0))
  
  (define (check-expmod-result result)
    (check-nontrivial-square-root result (remainder (square result) m)))
  
  (define (squaring-step)
    (check-expmod-result (expmod base (/ exp 2) m)))
  
  (cond ((= exp 0) 1)
        ((even? exp) (squaring-step))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
    
(define (total-miller-rabin-test n)
  (define (try-it a)
    (if (< a n)
        (if (= (expmod a (- n 1) n) 1)
            false
            (try-it (+ a 1)))
        true))
  (try-it 1))
  
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Result: Carmichael's numbers are correctly recognized as non-prime.