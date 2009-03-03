(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (add-interval x (negate-interval y)))

(define (negate-interval x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))

;; exercise 2.10
;Ben Bitdiddle, an expert systems programmer, looks over
;Alyssa\u2019s shoulder and comments that it is not clear what it means to divide by
;an interval that spans zero. Modify Alyssa\u2019s code to check for this condition
;and to signal an error if it occurs.

(define (div-interval x y)
  (if (spans? y 0)
      (error "denominator must not span 0")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (spans? i x)
  (and (<= (lower-bound i) x) (>= (upper-bound i) x)))

(define (error msg)
  (display msg))