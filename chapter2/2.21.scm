;Exercise 2.21: The procedure square-list takes a list of numbers as argument
;and returns a list of the squares of those numbers.
;       (square-list (list 1 2 3 4))
;       (1 4 9 16)
;Here are two di\ufb00erent de\ufb01nitions of square-list. Complete both of them by
;\ufb01lling in the missing expressions:
(define nil (list))

(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (square x)) items))

(define (square x) (* x x ))
