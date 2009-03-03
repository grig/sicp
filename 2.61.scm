;Exercise 2.61. Give an implementation of adjoin-set using the ordered representation. By analogy
;with element-of-set? show how to take advantage of the ordering to produce a procedure that
;requires on the average about half as many steps as with the unordered representation.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; worst-case (x is greater than every item in set) complexity is O(n)
;; best-case complexity is O(1)
;; on average, algorithm will require half as many steps as with unordered representation

(adjoin-set 1 '())
(adjoin-set 3 '(1 2 5 6))
(adjoin-set 8 '(1 2 5 6))