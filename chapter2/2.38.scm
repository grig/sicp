(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define nil '())

;Exercise 2.38: The accumulate procedure is also known as fold-right, be-
;cause it combines the \ufb01rst element of the sequence with the result of combining
;all the elements to the right. There is also a fold-left, which is similar to
;fold-right, except that it combines elements working in the opposite direction:
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;What are the values of

(equal? (fold-right / 1 (list 1 2 3)) (/ 3 2))

;; (1/(2/(3/1))) = 1/(2/3) = 3/2

(equal? (fold-left / 1 (list 1 2 3)) (/ 1 6))

;; (((1/1)/2)/3) = ((1/2)/3) = 1/6

(equal? (fold-right list nil (list 1 2 3)) '(1 (2 (3 ()))))

;; (list 1 (list 2 (list 3 nil))))
        
(equal? (fold-left list nil (list 1 2 3)) '(((() 1) 2) 3))

;; (list ((list (list nil 1) 2) 3)
;Give a property that op should satisfy to guarantee that fold-right and fold-
;left will produce the same values for any sequence.

;; op must be associative, that is, (a op b) op c must equal a op (b op c)