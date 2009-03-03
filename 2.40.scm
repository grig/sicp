(define nil '())

(define (enumerate-integers a b)
  (if (> a b)
      nil
      (cons a (enumerate-integers (+ a 1) b))))

(define (flatmap proc items)
  (accumulate append nil (map proc items)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;Exercise 2.40: Define a procedure unique-pairs that, given an integer n, gen-
;erates the sequence of pairs (i,j ) with 1 <= j < i <= n. Use unique-pairs to
;simplify the definition of prime-sum-pairs given above.

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-integers 1 (- i 1))))
           (enumerate-integers 1 n)))

;(define (prime-sum-pairs n)
;  (map make-pair-sum
;       (filter prime-sum? (unique-pairs n))))
