;Exercise 2.87. Install =zero? for polynomials in the generic arithmetic package. This will allow
;adjoin-term to work for polynomials with coefficients that are themselves polynomials.

;; returns true if coefficients of all terms are =zero?
(define (zero-poly? p)
  (define (iter terms)
    (if (empty-termlist? terms) #t
        (let ((t (first-term terms)))
          (if (=zero? (coeff t))
              (iter (rest-terms terms))
              #f)))))

(put '=zero? '(polynomial) zero-poly?)
