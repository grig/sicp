(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;Exercise 2.66. Implement the lookup procedure for the case where the set of records is structured as a
;binary tree, ordered by the numerical values of the keys.

(define (key node) (car node))

(define (lookup given-key set-of-records)
  (if (null? set-of-records) #f
      (let ((k (key (entry set-of-records))))
        (cond ((equal? k given-key) (entry set-of-records))
              ((< given-key k) (lookup given-key (left-branch set-of-records)))
              (else (lookup given-key (right-branch set-of-records)))))))

(define tree '((5 "c") ((1 "a") () ((3 "b") () ())) ((9 "e") ((7 "d") () ()) ((11 "f") () ()))))

(lookup 5 tree)
(lookup 11 tree)
(lookup 6 tree)