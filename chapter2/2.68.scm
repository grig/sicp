;Exercise 2.68. The encode procedure takes as arguments a message and a tree and produces the list of
;bits that gives the encoded message.
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;Encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a given
;symbol according to a given tree. You should design encode-symbol so that it signals an error if the
;symbol is not in the tree at all. Test your procedure by encoding the result you obtained in exercise 2.67
;with the sample tree and seeing whether it is the same as the original sample message.

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

;; samples
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (encode '(A B C D) sample-tree)
(equal? (encode '(A D A B B C A) sample-tree) sample-message)
; (encode '(A B D F A) sample-tree) ;=> error 