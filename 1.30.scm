;Exercise 1.30: The sum procedure above generates a linear recursion. The
;procedure can be rewritten so that the sum is performed iteratively. Show how
;to do this by \ufb01lling in the missing expressions in the following de\ufb01nition:
;      (define (sum term a next b)
;        (define (iter a result)
;           (if <?? >
;                <?? >
;                (iter <?? > <?? >)))
;        (iter <?? > <?? >))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; test

(define (inc n) (+ 1 n))
(define (identity x) x)

; > (sum identity 1 inc 10)
; 55
