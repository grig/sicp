;Exercise 2.18: De\ufb01ne a procedure reverse that takes a list as argument and
;returns a list of the same elements in reverse order:
;      (reverse (list 1 4 9 16 25))
;      (25 16 9 4 1)


;; uses append
(define (reverse lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

;; iterative version
(define (reverse1 lst)
  (define (reverse-iter acc lst)
    (if (null? lst)
        acc
        (reverse-iter (cons (car lst) acc) (cdr lst))))
  (reverse-iter (list) lst))

