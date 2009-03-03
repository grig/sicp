(define false #f)
(define true #t)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;Exercise 2.59. Implement the union-set operation for the unordered-list representation of sets.

(define (union-set set1 set2)
  (if (null? set1) 
      set2
      (let ((subresult (union-set (cdr set1) set2)))
        (if (element-of-set? (car set1) subresult)
            subresult
            (cons (car set1) subresult)))))

(equal? (union-set '() '(a b)) '(a b))
(equal? (union-set '(a b) '()) '(a b))
(element-of-set? 'a (union-set '(a b) '(c a)))
(element-of-set? 'b (union-set '(a b) '(c a)))
(element-of-set? 'c (union-set '(a b) '(c a)))