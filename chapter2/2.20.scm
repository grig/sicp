(define (same-parity i . rest)
  (define (filter lst)
    (if (null? lst)
        lst
        (if (even? (+ i (car lst))) ;; a + b is even <=> both a and b are even or a and b are both odd
            (cons (car lst) (filter (cdr lst)))
            (filter (cdr lst)))))
  (cons i (filter rest)))