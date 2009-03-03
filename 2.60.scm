;Exercise 2.60. We specified that a set would be represented as a list with no duplicates. Now suppose we
;allow duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2).
;Design procedures element-of-set?, adjoin-set, union-set, and intersection-set that
;operate on this representation. How does the efficiency of each compare with the corresponding procedure
;for the non-duplicate representation? Are there applications for which you would use this representation in
;preference to the non-duplicate one?

(define true #t)
(define false #f)

; O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

; operates in O(1)
(define (adjoin-set x set)
  (cons x set))

; O(n^2), same as before. However, lists would typically be larger, so intersection would be less efficient in duplicate case.
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (adjoin-set (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; O(n), where n is the size of set1
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (adjoin-set (car set1) (union-set (cdr set1) set2))))

(equal? (union-set '() '(a b)) '(a b))
(equal? (union-set '(a b) '()) '(a b))
(element-of-set? 'a (union-set '(a b) '(c a)))
(element-of-set? 'b (union-set '(a b) '(c a)))
(element-of-set? 'c (union-set '(a b) '(c a)))

(element-of-set? 'a (intersection-set '(a b) '( b c a)))