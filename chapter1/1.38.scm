;(define (incf f) (lambda (i) (f (+ 1 i))))

; recursive version
;(define (cont-frac n d k)
  ; may also use inner function with extra index but chose to offset generators of n and d instead.
;  (if (= k 1)
;      (/ (n k) (d k))
;      (/ (n 1) (+ (d 1) (cont-frac (incf n) (incf d) (- k 1))))))


(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (d i)
  (if (= (remainder i 3) 2)
      (* 2 (/ (+ 1 i) 3))
      1))

(define (approx-e n)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  d
                  n)))