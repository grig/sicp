;; average-damping
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average a b) (/ (+ a b) 2))

;; repeated
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
;      f
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

;; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  
  (define (try guess count)
    (display (list guess count))
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))
  (try first-guess 1))

(define (loga a x)
  (/ (log x) (log a)))

(define (log2 x) (loga 2 x))

(define (root x n)
  (define (inverse-exp y) (/ x (expt y (- n 1))))
  (let ((damps (floor (log2 n))))
    (let ((damp (repeated average-damp damps)))
      (fixed-point (damp inverse-exp) 1.0))))

;; hypothesis: root requires number of damps = floor(log_2(n))
;; proof?

;допустим, что для n = 2^k достаточно k приглушений. Докажем, что для n = 2^(k + 1) нужно k+1 приглушение:
;
;x = y^(2^(k + 1)) = y^(2*2^k) = (y^2)^(2^k)
;для вычисления y^2 нужно k приглушений, следовательно, для вычисления y необходимо k+1 приглушение.