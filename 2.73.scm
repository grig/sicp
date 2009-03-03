;; aux
(define *dispatch-table* (make-hash-table 'equal))
(define (put operation type item)
  (hash-table-put! *dispatch-table* (list operation type) item))
(define (get operation type)
  (hash-table-get *dispatch-table* (list operation type)))


(define (attach-tag tag contents)
  (cons tag contents))
(define (type-tag tagged-value) (car tagged-value))
(define (contents tagged-value) (cdr tagged-value))

(define (variable? x) (symbol? x))
(define (same-variable? x y) (eq? x y))

;Exercise 2.73. Section 2.3.2 described a program that performs symbolic differentiation:
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
;        <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))
;We can regard this program as performing a dispatch on the type of the expression to be differentiated. In
;this situation the ``type tag'' of the datum is the algebraic operator symbol (such as +) and the operation
;being performed is deriv. We can transform this program into data-directed style by rewriting the basic
;derivative procedure as
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;a. Explain what was done above. Why can't we assimilate the predicates number? and same-variable?
;into the data-directed dispatch?

;; we could have invented a (unit) pseudo-operator which simply returns its operand, but that would
;; either require us to specify this operator for primitive expressions, or unnecessarily complicate 
;; the (operator) function.

;b. Write the procedures for derivatives of sums and products, and the auxiliary code required to install
;them in the table used by the program above.

(define (make-sum x y) (list '+ x y))
(define (addend x) (car x))
(define (augend x) (cadr x))
(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (make-product x y) (list '* x y))
(define (multiplicand exp) (car exp))
(define (multiplier exp) (cadr exp))
(define (deriv-product exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))

(define (install-derivs)
  (put 'deriv '* deriv-product)
  (put 'deriv '+ deriv-sum)
  'done)

;c. Choose any additional differentiation rule that you like, such as the one for exponents (exercise 2.56),
;and install it in this data-directed system.

(define (deriv-exp exp var)
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (define (make-exp base exp) (list '** base exp))
  
  (if (and (variable? (base exp)) (same-variable? (base exp) var))
      (make-product (exponent exp) (make-exp (base exp) (make-sum (exponent exp) -1)))
      0))

(define (install-derivs)
  (put 'deriv '* deriv-product)
  (put 'deriv '+ deriv-sum)
  (put 'deriv '** deriv-exp)
  'done)

;d. In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it
;together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in
;deriv looked like
;((get (operator exp) 'deriv) (operands exp) var)
;What corresponding changes to the derivative system are required?

;; only chnage (install-derivs) to 
;(define (install-derivs)
;  (put '* 'deriv deriv-product)
;  (put '+ 'deriv deriv-sum)
;  (put '** 'deriv deriv-exp)
;  'done)
