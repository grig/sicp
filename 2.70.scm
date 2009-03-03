;Exercise 2.70. The following eight-symbol alphabet with associated relative frequencies was designed to
;efficiently encode the lyrics of 1950s rock songs. (Note that the ``symbols'' of an ``alphabet'' need not be
;individual letters.)
;
;A 2 NA 16
;BOOM 1 SHA 3
;GET 2 YIP 9
;JOB 2 WAH 1
;
;Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree, and use
;encode (exercise 2.68) to encode the following message:
;
;Get a job
;Sha na na na na na na na na
;Get a job
;Sha na na na na na na na na
;Wah yip yip yip yip yip yip yip yip yip
;Sha boom

(define alphabet '((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16)))
(define message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(define (test)
  (let ((song-tree (generate-huffman-tree alphabet)))
    (let ((encoded-message (encode message song-tree)))
      (display (equal? (decode encoded-message song-tree) message))
      (newline)
      (display (length encoded-message)))))


;How many bits are required for the encoding? 

;; 82 bits

; What is the smallest number of bits that would be needed to
; encode this song if we used a fixed-length code for the eight-symbol alphabet?

;; each 'symbol' would take 3 bits. There are

(length message)

;; 35 symbols, so whole message would take

(* 3 (length message))

;; 105 bits

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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge branches)
  (if (null? (cdr branches)) 
      (car branches)
      (successive-merge (adjoin-set (make-code-tree (car branches) (cadr branches)) (cddr branches)))))
