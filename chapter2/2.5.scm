(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (factor x base)
  (define (iter x count)
    (if (= (remainder x base) 0)
        (iter (/ x base) (+ count 1))
        count))
  (iter x 0))

(define (car z)
  (factor z 2))

(define (cdr z)
  (factor z 3))
