(define true #t)
(define false #f)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;Exercise 2.62. Give a O(n) implementation of union-set for sets represented as ordered lists.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                ((< x2 x1) (cons x2 (union-set set1 (cdr set2)))))))))

(equal? (union-set '() '(2 5 )) '(2 5))
(equal? (union-set '(2 5) '()) '(2 5))
(element-of-set? 1 (union-set '(2 5) '(1 2)))
(element-of-set? 2 (union-set '(2 5) '(1 2)))
(element-of-set? 5 (union-set '(2 5) '(1 2)))
