(define *dispatch-table* (make-hash-table 'equal))
(define (put operation type item)
  (hash-table-put! *dispatch-table* (list operation type) item))
(define (get operation type)
  (hash-table-get *dispatch-table* (list operation type)))

(define (apply-generic op . args)
  (apply (get op (map type-tag args)) (map contents args)))

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
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;Exercise 2.78. The internal procedures in the scheme-number package are essentially nothing more
;than calls to the primitive procedures +, -, etc. It was not possible to use the primitives of the language
;directly because our type-tag system requires that each data object have a type attached to it. In fact,
;however, all Lisp implementations do have a type system, which they use internally. Primitive predicates
;such as symbol? and number? determine whether data objects have particular types. Modify the
;definitions of type-tag, contents, and attach-tag from section 2.4.2 so that our generic system
;takes advantage of Scheme's internal type system. That is to say, the system should work as before except
;that ordinary numbers should be represented simply as Scheme numbers rather than as pairs whose car is
;the symbol scheme-number.

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

(install-scheme-number-package)