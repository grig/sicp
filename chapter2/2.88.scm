;Exercise 2.88. Extend the polynomial system to include subtraction of polynomials. (Hint: You may find
;it helpful to define a generic negation operation.)

(define (negate x) (apply-generic 'negate x))
(define (sub x y) (add x (negate y)))

;; install-polynomial-package
(define (negate-poly p)
  (make-poly (variable p)
             (map negate-term (term-list p))))

(define (negate-term t)
  (make-term (order t) (negate (coeff t))))

(put 'negate '(polynomial) negate-poly)

;; existing definitions
;;
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))