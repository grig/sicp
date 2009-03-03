(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; Exercise 2.63. Each of the following two procedures converts a binary tree to a list.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;a. Do the two procedures produce the same result for every tree? If not, how do the results differ? What
;lists do the two procedures produce for the trees in figure 2.16?

;; seems like they do produce the same result.

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(map equal? (map tree->list-1 (list tree1 tree2 tree3))
     (map tree->list-2 (list tree1 tree2 tree3)))

; b. Do the two procedures have the same order of growth in the number of steps required to convert a
; balanced tree with n elements to a list? If not, which one grows more slowly?

;; tree->list-1
;;
;; append takes number of steps proportional to length of the first argument. Let c(n) == c*n be that number. Total number of steps required for converting a balanced tree of size n would then be
;; S(N) = 2*S(N/2) + c*N/2
;; S(N) = 2*S(N/2) + c*N/2 = 4*S(N/4) + 2c*(N/2) = 8*S(N/8) + 4c*N/8 + 2c*(N/2) = 8*S(N/8) + 3cN/2 = 
;;       = .. = m*s(n/m) + (log m)cn/2
;; let s(1) = k = const; substituting m for n we get
;; S(N) = n*s(1) + (1/2)cn(log n) = n(k + c'log(n))
;;
;; Growth will be O(nlog(n)).

;; tree->list-2
;; 
;; For each node copy-to-list first calls itself recursively on the right branch, then on the left branch. Therefore,
;; assuming a balanced tree, total number of steps will equal
;; S(N) = k + 2*S(N/2), where k is a constant overhead for each recursive step.
;; S(N) = k + 2*S(N/2) = k + 2k + 4S(N/4) = k + 2k + 2^2*k + 2^3*S(N/2^3) = k + 2k + 2^2k + 2^3k + 2^4S(N/2^4) = .. = (2^m - 2)/2 + 2^m*S(N/2^m) = 
;;      = 2^(m - 1) - 1 + 2^mS(N/2^m)
;; let m = log2(N), then S(N) = 2^(log2(N) - 1) - 1 + 2^(log2(N))S(1) = N/2 - 1 + N*S(1)
;; 
;; Growth will be O(N)