;Exercise 2.31: Abstract your answer to unde\ufb01ned [Exercise 2-30], page un-
;de\ufb01ned to produce a procedure tree-map with the property that square-tree
;could be de\ufb01ned as
;      (define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (t)
         (if (pair? t) (tree-map proc t) (proc t)))
       tree))

(define (square-tree tree) (tree-map square tree))
(define (square x) (* x x))
