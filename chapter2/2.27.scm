;Exercise 2.27: Modify your reverse procedure of unde\ufb01ned [Exercise 2-18],
;page unde\ufb01ned to produce a deep-reverse procedure that takes a list as
;argument and returns as its value the list with its elements reversed and with
;all sublists deep-reversed as well. For example,
;       (define x (list (list 1 2) (list 3 4)))
;       x
;       ((1 2) (3 4))
;       (reverse x)
;       ((3 4) (1 2))
;       (deep-reverse x)
;       ((4 3) (2 1))

;; iterative version
(define (reverse lst)
  (define (reverse-iter acc lst)
    (if (null? lst)
        acc
        (reverse-iter (cons (car lst) acc) (cdr lst))))
  (reverse-iter (list) lst))

(define (deep-reverse lst)
  (define (iter acc lst)
    (if (null? lst)
        acc
        (let ((x (car lst)))
          (let ((r (if (pair? x) (deep-reverse x) x)))
              (iter (cons r acc) (cdr lst))))))
  (iter (list) lst))
