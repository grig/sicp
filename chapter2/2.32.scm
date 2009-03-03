;Exercise 2.32: We can represent a set as a list of distinct elements, and we can
;represent the set of all subsets of the set as a list of lists. For example, if the
;set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2)
;(1 2 3)). Complete the following de\ufb01nition of a procedure that generates the
;set of subsets of a set and give a clear explanation of why it works:

;; (subsets '()) => (())
;; (subsets '(1)) => (() (1)) = (append (subsets '()) ((1)))

(define nil '())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; subsets X = (()), if !exists x \belongs X;
;;           = { y \union {()} | y \belongs subsets(X\{x}) \union 
;;             { y \union {x}  | y \belongs subsets(X\{x}))
;;
;; alternatively, if we encode each subset as a characteristic function of set's elements, than subsets(X) = (all functions from subsets(X\x) extended on x with 0) union (all functions ... extended on x with 1).