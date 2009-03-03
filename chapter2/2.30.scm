;Exercise 2.30: De\ufb01ne a procedure square-tree analogous to the square-list
;procedure of unde\ufb01ned [Exercise 2-21], page unde\ufb01ned . That is, square-
;list should behave as follows:
;      (square-tree
;       (list 1
;               (list 2 (list 3 4) 5)
;               (list 6 7)))
;      (1 (4 (9 16) 25) (36 49))
;Define square-tree both directly (i.e., without using any higher-order proce-
;dures) and also by using map and recursion.

(define nil '())
(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))


(define (square-tree1 tree)
  (map (lambda (t)
         (if (pair? t) (square-tree1 t) (square t)))
       tree))