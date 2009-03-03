;Exercise 2.28: Write a procedure fringe that takes as argument a tree (repre-
;sented as a list) and returns a list whose elements are all the leaves of the tree
;arranged in left-to-right order. For example,
;      (define x (list (list 1 2) (list 3 4)))
;      (fringe x)
;      (1 2 3 4)
;      (fringe (list x x))
;      (1 2 3 4 1 2 3 4)


;(fringe t) == (append (fringe x) (fringe y) (fringe z))

;; takes a list of lists and catenates all it elements into one list.
(define (join lsts)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (append acc (car rest)) (cdr rest))))
  (iter (list) lsts))
           
(define (fringe tree)
  (define (branch-or-leaf t)
    (if (pair? t)
        (fringe t)
        (list t)))
  
  (join (map branch-or-leaf tree)))