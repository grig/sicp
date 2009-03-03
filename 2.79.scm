(define *dispatch-table* (make-hash-table 'equal))
(define (put operation type item)
  (hash-table-put! *dispatch-table* (list operation type) item))
(define (get operation type)
  (hash-table-get *dispatch-table* (list operation type)))

(define (apply-generic op . args)
  (apply (get op (map type-tag args)) (map contents args)))

(define (attach-tag tag contents)
  (if (eq? tag 'scheme-number) 
      contents 
      (cons tag contents)))

(define (type-tag tagged-value) 
  (cond ((number? tagged-value) 'scheme-number)
        (else (car tagged-value))))

(define (contents tagged-value) 
  (cond ((number? tagged-value) tagged-value)
        (else (cdr tagged-value))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; rationals
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  (put 'equ? '(scheme-number rational)
       (lambda (x y) (equ? (make-rational x 1) (tag y))))
  (put 'equ? '(rational scheme-number)
       (lambda (x y) (equ? y (tag x))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))


(install-scheme-number-package)
(install-rational-package)

;Exercise 2.79. Define a generic equality predicate equ? that tests the equality of two numbers, and install
;it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers,
;and complex numbers.

(define (equ? x y) (apply-generic 'equ? x y))
