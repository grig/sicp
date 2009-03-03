(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;Exercise 2.64. The following procedure list->tree converts an ordered list to a balanced binary tree.
;The helper procedure partial-tree takes as arguments an integer n and list of at least n elements and
;constructs a balanced tree containing the first n elements of the list. The result returned by partialtree
;is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of
;elements not included in the tree.

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

;a. Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree
;produced by list->tree for the list (1 3 5 7 9 11).

;; each recursive step takes first n elements and divides them in two halves plus one element. It then applies itself recursively on the first half, producing a pair of a left branch and the rest elements. A head of the rest is taken as a root node, and the procedure then constructs a right branch from the rest of the elements. It returns a tree constructed from those branches and a root node, consed with a rest of the list.

(list->tree '(1 3 5 7 9 11))
;; (p '(1 3 5 7 9 11) 6)
; (p '(1 3 .. ) 2) = (1 () (p '(3) 1) = (1 () (3 () ())) . (5 7 9 11)

;; (5 (1 () (3 () ())) (p '(7 9 11)))

; (p '(7 9 11)) = (9 (car (p (7) 1) (car (p (11) 1))) = (9 (7 () ()) (11 () ()))

(equal? (list->tree '(1 3 5 7 9 11)) '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))))

;b. What is the order of growth in the number of steps required by list->tree to convert a list of n
;elements?

;; S(N) = S(N/2 - 1) + k + S(N/2 - 1) = k + 2S(N/2 - 1). S is monotonic, then
;; k + 2S(N/2 - 2) < k + 2S(N/2 - 1) < k + 2S(N/2) = N/2 - 1 + NS(1)
;; k + 2S(N/2 - 2) = k + 2k + 4S(N/4 - 2) = k + 2k + 4k + 8S(N/8 - 2) = .. = k + 2k + .. + 2^(m - 1)k + 2^mS(N/(2^m) - 2) = (2^m - 1)/2 + 2^mS(N/2^m - 2)
;; N/2^m -2 = 0
;; N = 2*2^m => m = log2(N) - 1; 2^m = (2^log2(N))/2 = N/2
;;   substituting, we get
;; S'(N) = N/4 - 1/2 + N/2*S(0) and has growth of O(N).

;; Therefore, S(N) has growth of O(N).