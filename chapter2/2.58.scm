(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; Simplifications

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation base exp)
  (list '** base exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)         
         (let ((base (base exp))
               (n (exponent exp)))
           (if (number? n)
               (make-product 
                (make-product n (make-exponentiation base (- n 1)))
                (deriv base var))
               (error "don't know how to derive an exponentiation with variable exponent" var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-exponentiation base expt)
  (cond ((=number? expt 1) base)
        ((=number? expt 0) 1)
        (else (list '** base expt))))
;
;Exercise 2.58. Suppose we want to modify the differentiation program so that it works with ordinary
;mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation
;program is defined in terms of abstract data, we can modify it to work with different representations of
;expressions solely by changing the predicates, selectors, and constructors that define the representation of
;the algebraic expressions on which the differentiator is to operate.
;
;a. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x +
;(3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments and
;that expressions are fully parenthesized.

(define (sum? e)
  (and (pair? e) (pair? (cdr e)) (eq? (cadr e) '+)))

(define (addend x) (car x))
(define (augend x) (caddr x))
(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list a '+ b))))

(define (product? e)
  (and (pair? e) (pair? (cdr e)) (eq? (cadr e) '*)))

(define (multiplier x) (car x))
(define (multiplicand x) (caddr x))
(define (make-product a b) 
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((and (number? a) (number? b)) (* a b))
        ((=number? a 1) b)
        ((=number? b 1) a)
        (else (list a '* b))))

(deriv '(x + (x * (x * 5))) 'x)

;b. The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 *
;(x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done before
;addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our
;derivative program still works?

(define (member? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (else (member? x (cdr lst)))))

(define (sum? x)
  (and (pair? x) (member? '+ x)))

(define (until x lst)
  (define (iter x lst acc)
    (if (null? lst) '()
        (if (eq? x (car lst)) acc
            (iter x (cdr lst) (cons (car lst) acc)))))
  (reverse (iter x lst '())))

(define (after x lst)
  (if (null? lst) '()
      (if (eq? x (car lst))
          (cdr lst)
          (after x (cdr lst)))))

(define (addend x)
  (simplify-variables (until '+ x)))

(define (augend x)
  (simplify-variables (after '+ x)))

(define (product? x) (member? '* x))
(define (multiplicand x) (simplify-variables (until '* x)))
(define (multiplier x) (simplify-variables (after '* x)))

(define (simplify-variables lst)
  (if (null? (cdr lst))
      (car lst)
      lst))

(define my-lst '(x * y * (x + y) + z + z))
(until '+ '(x * y + z))
(until '+ my-lst)
(after '+ my-lst)
          
(eq? (sum? '(x * y + z)) #t)
(eq? (sum? '(x * (y + z))) #f)
(equal? (addend '(x + y)) 'x)
(equal? (addend '(x * y + z)) '(x * y))
(equal? (augend '(x + y)) 'y)
(equal? (augend '(x * y + z + x)) '(z + x))

(deriv '(x * x * x * x + x) 'x)