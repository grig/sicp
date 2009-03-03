;Exercise 2.69. The following procedure takes as its argument a list of symbol-frequency pairs (where no
;symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman
;algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of
;leaves. Successive-merge is the procedure you must write, using make-code-tree to successively
;merge the smallest-weight elements of the set until there is only one element left, which is the desired
;Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a
;complex procedure, then you are almost certainly doing something wrong. You can take significant
;advantage of the fact that we are using an ordered set representation.)

(define (successive-merge branches)
  (if (null? (cdr branches)) 
      (car branches)
      (successive-merge (adjoin-set (make-code-tree (car branches) (cadr branches)) (cddr branches)))))

;; definitions
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree) '()
      (if (contains-symbol? symbol tree)
          (cond ((contains-symbol? symbol (left-branch tree)) (cons 0 (encode-symbol symbol (left-branch tree))))
                ((contains-symbol? symbol (right-branch tree)) (cons 1 (encode-symbol symbol (right-branch tree)))))
          (error "tree does not contain symbol" symbol))))

(define (contains-symbol? symbol tree)
  (define (find-in-set item set)
    (if (null? set) #f
        (if (equal? item (car set)) #t
            (find-in-set item (cdr set)))))
  (find-in-set symbol (symbols tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

; samples
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-tree-2 (generate-huffman-tree '((C 1) (D 1) (B 2) (A 4))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(equal? (encode '(A B C D) sample-tree) (encode '(A B C D) sample-tree-2))
(equal? (decode sample-message sample-tree) (decode sample-message sample-tree-2))

; (equal? (encode '(A D A B B C A) sample-tree) sample-message)

