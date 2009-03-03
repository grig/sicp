;Exercise 2.17: De\ufb01ne a procedure last-pair that returns the list that contains
;only the last element of a given (nonempty) list:
;      (last-pair (list 23 72 149 34))
;      (34)

(define (last-pair lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (last-pair (cdr lst))))