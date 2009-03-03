;Exercise 2.89. Define procedures that implement the term-list representation described above as
;appropriate for dense polynomials.

;; 2x^3 + x + 1 will be represented as (2 0 1 1)
(define (the-empty-termlist) '())
(define (first-term term-list) (make-term (length (cdr term-list)) (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cond ((= (order term) (length term-list)) (cons (coeff term) term-list))
            ((> (order term) (length term-list)) (adjoin-term term (cons 0 term-list)))
            (else (error "term has lower order than term-list" (list term term-list))))))

;; stub
(define (=zero? x) (= 0 x))
