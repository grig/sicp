;Exercise 2.82. Show how to generalize apply-generic to handle coercion in the general case of
;multiple arguments. One strategy is to attempt to coerce all the arguments to the type of the first argument,
;then to the type of the second argument, and so on. Give an example of a situation where this strategy (and
;likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case where
;there are some suitable mixed-type operations present in the table that will not be tried.)

(define (apply-generic op . args)
  (define (try-coercion coercion-options)
    (if (null? coercion-options)
        (error "no suitable operations found" (list op args))
        (let ((coerce-to (car coercion-options)))
          (let ((coerced-args (map (lambda (x) (let ((x->c (get-coercion (type-tag x) coerce-to))) 
                                                 (if x->c (x->c x) x))) 
                                   args)))
            (let ((proc (get op (map type-tag coerced-args))))
              (if proc
                  (apply proc (map contents coerced-args))
                  (try-coercion (cdr coercion-options))))))))
  
  (let ((proc (get op (map type-tag args))))
    (if proc
      (apply proc (map contents args))
      (try-coercion (map type-tag args)))))

(define (test)
  (put-coercion 'scheme-number 'rational (lambda (n) (make-rational (contents n) 1)))
  (add 1 (make-rational 2 3)))

(define (add* . args)
  (if (null? args) 0
      (add (car args) (apply add* (cdr args)))))

;; Suppose we know how to take a natural power of a rational number, that is, (1/3)**3, but don't yet know how to exponentiate two rational numbers.
;;
;; that is, install-rational-package defines 
;;
;; (put 'exp '(rational scheme-number) (lambda (r n) (tag (make-rat (expt (numer r) n) (expt (denom r) n))) 
;;
;; although exp is not defined for simply scheme numbers, because previous case should cover that already.

(define (exp base exponent) (apply-generic 'exp base exponent))

;; so, (exp 2 2) should work but it doesn't.

;;;;;;;;;;;;;;;;;
;; definitions ;;
;;;;;;;;;;;;;;;;;
(define *dispatch-table* (make-hash-table 'equal))
(define (put operation type item)
  (hash-table-put! *dispatch-table* (list operation type) item))
(define (get operation type)
  (hash-table-get *dispatch-table* (list operation type) #f))

(define *coercion-table* (make-hash-table 'equal))
(define (put-coercion from to proc)
  (hash-table-put! *coercion-table* (list from to) proc))
(define (get-coercion from to)
  (hash-table-get *coercion-table* (list from to) #f))

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
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
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
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'exp '(rational scheme-number) 
       (lambda (r n) (tag (make-rat (expt (numer r) n) (expt (denom r) n)))))
  'rational)
(define (make-rational n d)
  ((get 'make 'rational) n d))


(install-scheme-number-package)
(install-rational-package)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'rectangular)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'polar)
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(install-polar-package)
(install-rectangular-package)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'complex)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)