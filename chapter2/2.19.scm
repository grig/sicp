(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

;Does the order of the list coin-values affect the answer produced by cc? 
;Why or why not?

;                                (3 (1 2 3))
;               (3 (2 3))                       (2 (1 2 3)))
;         (3 (3))       (1 (2 3))             (2 (2 3))            (1 (1 2 3))
;     (3 ()) (0 (3))    (1 (3)) (-1 (2 3))  (2 (3)) (0 (3))    (1 (2 3)) (0 (2 3)
;       0       1   (1 ()) (-2 (3)) 0     (2 ()) (-1 (3) 1     (1 (3)) (-1 (2 3)) 1 
;                     0       0             0       0        (1 ()) (-2 (3))  0
;                                                               0       0
;                                                               
;                                     (3 (3 1 2))
;                  (3 (1 2))                              (0 (3 1 2))
;           (3 (2))              (2 (1 2))                    1
;      (3 ())   (1 (2))        (2 (2))        (1 (1 2))   
;        0   (1 ()) (-1 (2))  (2 ()) (0 (2)) (1 (2))         (0 (1 2))    
;               0     0          0      1   (1 ()) (-1 (2))      1
;                                              0       0                

;; By induction: suppose that counting change of (k < n) do not depend on the order, whatever the list of coin values actually is.
;; That is true for for n = 0.
;; cc(c, list) = cc(c, (cdr list)) + cc(c - (car list), list)
;; second operand does not depend on list order by proposition.
;; Reducing the first operand further, we will eventually get the following:
;; cc(c, (cdr list)) = cc(c, ()) + cc(c - 1, ...) + ...
;; All other components of the sum do not depend on order of coin values, 
;; and cc(c, ()) equals zero.
;;
;; Thus, by induction, for all n, for all lists cc(n, list) does not depend on the order of elements in list.