(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


;Exercise 2.65. Use the results of exercises 2.63 and 2.64 to give O(n) implementations of union-set
;and intersection-set for sets implemented as (balanced) binary trees.

;; use implementations of intersection-set and union-set for ordered lists behind the scenes.

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-ordered (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-ordered (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-ordered set1 (cdr set2)))))))

(define (intersection-set set1 set2)
  (intersection-set-ordered (tree->list set1) (tree->list set2)))

(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2) (cons x1 (union-set-ordered (cdr set1) (cdr set2))))
                ((< x1 x2) (cons x1 (union-set-ordered (cdr set1) set2)))
                ((< x2 x1) (cons x2 (union-set-ordered set1 (cdr set2)))))))))

(define (union-set set1 set2)
  (list->tree (union-set-ordered (tree->list set1) (tree->list set2))))

;; examples

(define tree1 (list->tree '(1 3 5 7 9 11 13)))
(define tree2 (list->tree '(2 4 6 8 10 12)))

(intersection-set tree1 tree2)
(tree->list (union-set tree1 tree2))
(display "root: ")
(display (car (union-set tree1 tree2)))
(display "\n")

;; intersection-set-ordered and union-set-ordered are O(n), conversion routines are O(n) therefore intersection-set and union-set are O(n) themselves.